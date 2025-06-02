import { FC, useContext, useState } from 'react';
import './ImportTranscriptPopup.scss';
import { Button, Form, Modal } from 'react-bootstrap';
import { addRoadmapPlan, RoadmapPlan, selectAllPlans, setPlanIndex } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { parse as parseHTML, HTMLElement } from 'node-html-parser';
import ThemeContext from '../../style/theme-context';
import { BatchCourseData, PlannerQuarterData, PlannerYearData } from '../../types/types';
import { quarters } from '@peterportal/types';
import { searchAPIResults } from '../../helpers/util';
import { QuarterName } from '@peterportal/types';
import { makeUniquePlanName, normalizeQuarterName } from '../../helpers/planner';
import {
  setUserAPExams,
  setTransferredCourses,
  setUncategorizedCourses,
} from '../../store/slices/transferCreditsSlice';
import { useTransferredCredits } from '../../hooks/transferCredits';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import trpc from '../../trpc';

import DescriptionIcon from '@mui/icons-material/Description';

interface TransferUnitDetails {
  date: string;
  name: string;
  raw: string;
  score: number;
  units: number;
}

interface TranscriptCourse {
  title: string;
  dept: string;
  code: string;
  units: number;
  grade: string;
  gradePts: number;
}

interface TranscriptQuarter {
  name: string;
  courses: TranscriptCourse[];
}

function toCourseID(course: TranscriptCourse) {
  return (course.dept + course.code).replace(/\s/g, '');
}

function processSchoolsRows(rows: HTMLElement[], transfers: TransferUnitDetails[]) {
  for (const row of rows) {
    const text = row.text.trim();
    if (!text) continue;
    if (row.classList.contains('title')) return;

    const [raw, name, score, units, date] =
      text.match(/^((?:.(?!\(Score|\(Units))+)\s\((?:Score\s(\d))?(?:,\s)?Units ([\d.]+)\).*?(\d+\/\d+)/) || [];
    if (!raw) continue;
    transfers.push({ raw, name, score: Number(score), units: Number(units), date });
  }
}

function processQuartersRow(row: HTMLElement, quarters: TranscriptQuarter[]): void {
  const text = row.text.trim();
  if (!text) return;

  if (row.classList.contains('title')) {
    quarters.push({ name: text, courses: [] });
    return;
  }

  if (!row.classList.contains('grades')) return;

  // This is a course taken at UCI, add it to the last seen quarter title
  const cols = row.childNodes.filter((x) => x.nodeType === 1).slice(1);
  const [title, dept, code, units, grade, gradePts] = cols.map((x) => x.text.trim());

  // Add this course to the current (last) item in the quarters array
  quarters.at(-1)!.courses.push({ title, dept, code, units: Number(units), grade, gradePts: Number(gradePts) });
}

async function htmlFromFile(file: Blob): Promise<HTMLElement> {
  const reader = new FileReader();
  const fileText = (await new Promise((resolve) => {
    reader.addEventListener('load', () => resolve(reader.result as string));
    reader.readAsText(file);
  })) as string;
  return parseHTML(fileText);
}

async function transcriptCourseDetails(quarters: TranscriptQuarter[]): Promise<BatchCourseData> {
  const courseIDs = quarters.flatMap((q) => q.courses).map(toCourseID);
  const results = await searchAPIResults('courses', courseIDs);
  return results;
}

function toPlannerQuarter(
  quarter: TranscriptQuarter,
  courses: BatchCourseData,
): { startYear: number; quarterData: PlannerQuarterData } {
  const year = parseInt(quarter.name.split(' ')[0]);
  // Removes the year number and "Session/Quarter" at the end
  const tName = quarter.name.split(' ').slice(1, -1).join(' ');
  const name = normalizeQuarterName(tName);

  return {
    startYear: name === 'Fall' ? year : year - 1,
    quarterData: { name, courses: quarter.courses.map((c) => courses[toCourseID(c)]) },
  };
}

function filterOutInvalidCourses(quarters: TranscriptQuarter[], courses: BatchCourseData) {
  const validatedQuarters: TranscriptQuarter[] = [];
  const invalidCourseIDs: string[] = [];

  for (const q of quarters) {
    const validatedQuarter: TranscriptQuarter = { ...q, courses: [] };
    for (const c of q.courses) {
      if (toCourseID(c) in courses) {
        validatedQuarter.courses.push(c);
      } else {
        invalidCourseIDs.push(toCourseID(c));
      }
    }
    validatedQuarters.push(validatedQuarter);
  }

  return { validatedQuarters, invalidCourseIDs };
}

function groupIntoYears(qtrs: { startYear: number; quarterData: PlannerQuarterData }[]) {
  const years = qtrs.reduce(
    (years, q) => {
      const yearIdx = Object.keys(years).length + 1;

      if (years[q.startYear]) years[q.startYear].quarters.push(q.quarterData);
      else years[q.startYear] = { startYear: q.startYear, name: 'Year ' + yearIdx, quarters: [q.quarterData] };

      return years;
    },
    {} as { [k: string]: PlannerYearData },
  );
  const baseQtrs: QuarterName[] = ['Fall', 'Winter', 'Spring'];
  Object.values(years).forEach((year) => {
    baseQtrs.forEach(
      (name) => !year.quarters.find((q) => q.name === name) && year.quarters.push({ name, courses: [] }),
    );
    year.quarters.sort((a, b) => quarters.indexOf(a.name) - quarters.indexOf(b.name));
  });
  return years;
}

async function processTranscript(file: Blob) {
  const doc = await htmlFromFile(file);

  const quartersRows = [...doc.querySelectorAll('#chrono-view .lineItems tr')];
  const schoolsRows = [...doc.querySelectorAll('#school-view .lineItems tr')];
  const transfers: TransferUnitDetails[] = [];
  const quarters: TranscriptQuarter[] = [];

  // Process the rows in the transcript and create a course lookup by code
  quartersRows.forEach((r) => processQuartersRow(r, quarters));
  processSchoolsRows(schoolsRows, transfers);

  const courses = await transcriptCourseDetails(quarters);

  const { validatedQuarters, invalidCourseIDs } = filterOutInvalidCourses(quarters, courses);

  // Create the planner quarter format (with course details) by using the
  // course lookup and the grouped quarters in the transcript
  const plannerQuarters = validatedQuarters.map((q) => toPlannerQuarter(q, courses));
  plannerQuarters.sort((a, b) => a.startYear - b.startYear);

  const years = groupIntoYears(plannerQuarters);

  return { transfers, years, invalidCourseIDs };
}

async function organizeTransfers(transfers: TransferUnitDetails[]) {
  const mapped = transfers.map((transfer) => ({ name: transfer.name, units: transfer.units, score: transfer.score }));
  const response = await trpc.transferCredits.convertUserLegacyTransfers.query(mapped);
  return response;
}

const ImportTranscriptPopup: FC = () => {
  const { darkMode } = useContext(ThemeContext);
  const [showModal, setShowModal] = useState(false);
  const allPlanData = useAppSelector(selectAllPlans);
  const [file, setFile] = useState<Blob | null>(null);
  const [filePath, setFilePath] = useState('');
  const [busy, setBusy] = useState(false);
  const isLoggedIn = useIsLoggedIn();

  const currentAps = useTransferredCredits().ap;
  // App selector instead of useTransferredCredits.courses here because
  // useTransferredCredits.courses also includes all courses that have been cleared
  const currentCourses = useAppSelector((state) => state.transferCredits.transferredCourses);
  const currentOther = useTransferredCredits().other;

  const dispatch = useAppDispatch();
  const importHandler = async () => {
    if (!file) return;
    setBusy(true);
    try {
      const { transfers, years, invalidCourseIDs } = await processTranscript(file);
      const { courses, ap, other } = await organizeTransfers(transfers);

      // Merge the new AP exams, courses, and other transfers into current transfers
      // via a process similar to the updated Zot4Plan imports
      const scoredAps = ap.map(({ score, ...other }) => ({ ...other, score: score ?? 1 }));
      const newAps = scoredAps.filter(
        (imported) => !currentAps.some((existing) => existing.examName == imported.examName),
      );
      const mergedAps = currentAps.concat(newAps);

      const newCourses = courses.filter(
        (imported) => !currentCourses.some((existing) => existing.courseName == imported.courseName),
      );
      const mergedCourses = currentCourses.concat(newCourses);

      const formattedOther = other.map(({ courseName: name, units }) => ({ name, units }));
      const newOther = formattedOther.filter(
        (imported) => !currentOther.some((existing) => existing.name == imported.name),
      );
      const mergedOther = currentOther.concat(newOther);
      const newOtherFromCourses = invalidCourseIDs
        .map((courseID) => ({ name: courseID, units: 0 }))
        .filter((otherCourse) => !mergedOther.some((existing) => existing.name == otherCourse.name));
      const mergedOtherFinal = mergedOther.concat(newOtherFromCourses);

      // Override local transfers with the merged results
      dispatch(setTransferredCourses(mergedCourses));
      dispatch(setUserAPExams(mergedAps));
      dispatch(setUncategorizedCourses(mergedOtherFinal));

      // Add the new rows in the database if logged in
      if (isLoggedIn) {
        await trpc.transferCredits.overrideAllTransfers.mutate({
          courses: mergedCourses,
          ap: mergedAps,
          ge: [],
          other: mergedOtherFinal,
        });
      }

      const filename = filePath.replace(/.*(\\|\/)|\.[^.]*$/g, '');
      const newPlan: RoadmapPlan = {
        name: makeUniquePlanName(filename, allPlanData),
        content: { yearPlans: Object.values(years), invalidCourses: [] },
      };
      dispatch(addRoadmapPlan(newPlan));
      dispatch(setPlanIndex(allPlanData.length));

      setShowModal(false);
      setFile(null);
    } finally {
      setBusy(false);
    }
  };

  return (
    <>
      <Modal
        show={showModal}
        onHide={() => setShowModal(false)}
        centered
        className="ppc-modal multiplan-modal transcript-form"
      >
        <Modal.Header closeButton>
          <h2>Import from Transcript</h2>
        </Modal.Header>
        <Modal.Body>
          <Form className="ppc-modal-form">
            <Form.Group>
              Please upload an HTML copy of your unofficial transcript. To obtain this:
              <ol>
                <li>
                  Go to{' '}
                  <a href="https://www.reg.uci.edu/access/student/transcript/?seg=U" target="_blank" rel="noreferrer">
                    Student Access
                  </a>
                </li>
                <li>Navigate to "Unofficial Transcript"</li>
                <li>Save the page (ctrl/cmd + s)</li>
              </ol>
            </Form.Group>
            <Form.Group>
              <Form.Label className="ppc-modal-form-label">Transcript File</Form.Label>
              <Form.Control
                required
                type="file"
                name="transcript"
                accept="text/html"
                onInput={(e: React.FormEvent<HTMLInputElement>) => {
                  const input = e.target as HTMLInputElement;
                  setFile(input.files![0]);
                  setFilePath(input.value);
                }}
              ></Form.Control>
            </Form.Group>
          </Form>
          <Button variant="primary" disabled={!file || busy} onClick={importHandler}>
            {busy ? 'Importing...' : 'Import'}
          </Button>
        </Modal.Body>
      </Modal>
      <Button variant={darkMode ? 'dark' : 'light'} className="ppc-btn" onClick={() => setShowModal(true)}>
        <DescriptionIcon />
        <span>Student Transcript</span>
      </Button>
    </>
  );
};

export default ImportTranscriptPopup;
