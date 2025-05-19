import { FC, useContext, useState } from 'react';
import './ImportTranscriptPopup.scss';
import { FileEarmarkText } from 'react-bootstrap-icons';
import { Button, Form, Modal } from 'react-bootstrap';
import { setYearPlans } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import { parse as parseHTML, HTMLElement } from 'node-html-parser';
import ThemeContext from '../../style/theme-context';
import { BatchCourseData, PlannerQuarterData, PlannerYearData } from '../../types/types';
import { quarters } from '@peterportal/types';
import { searchAPIResults } from '../../helpers/util';
import { QuarterName, UserAPExam } from '@peterportal/types';
import { normalizeQuarterName } from '../../helpers/planner';
import { LocalTransferSaveKey, saveLocalTransfers } from '../../helpers/transferCredits';
import { TransferredCourse } from '../../store/slices/transferCreditsSlice';
import { UncategorizedCourseEntry } from '../../pages/RoadmapPage/transfers/UncategorizedCreditsSection';
import trpc from '../../trpc';

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

  // Create the planner quarter format (with course details) by using the
  // course lookup and the grouped quarters in the transcript
  const plannerQuarters = quarters.map((q) => toPlannerQuarter(q, courses));
  plannerQuarters.sort((a, b) => a.startYear - b.startYear);

  const years = groupIntoYears(plannerQuarters);
  return { transfers, years };
}

async function organizeTransfers(transfers: TransferUnitDetails[]) {
  const mapped = transfers.map((transfer) => ({ name: transfer.name, units: transfer.units, score: transfer.score }));
  const response = await trpc.transferCredits.convertUserLegacyTransfers.query(mapped);
  return response;
}

const ImportTranscriptPopup: FC = () => {
  const { darkMode } = useContext(ThemeContext);
  const [showModal, setShowModal] = useState(false);
  const [file, setFile] = useState<Blob | null>(null);
  const [busy, setBusy] = useState(false);

  const dispatch = useAppDispatch();
  const importHandler = async () => {
    if (!file) return;
    setBusy(true);
    try {
      const { transfers, years } = await processTranscript(file);
      const { courses, ap, other } = await organizeTransfers(transfers);

      // This section is repeated from planner, maybe refactor in the future
      const scoredAPs = ap.map(({ score, ...other }) => ({ ...other, score: score ?? 1 }));
      const formattedOther = other.map(({ courseName: name, units }) => ({ name, units }));

      saveLocalTransfers<TransferredCourse>(LocalTransferSaveKey.Course, courses);
      saveLocalTransfers<UserAPExam>(LocalTransferSaveKey.AP, scoredAPs);
      saveLocalTransfers<UncategorizedCourseEntry>(LocalTransferSaveKey.Uncategorized, formattedOther);

      dispatch(setYearPlans(Object.values(years)));
      setShowModal(false);
      setFile(null);
    } finally {
      setBusy(false);
    }
  };

  return (
    <>
      <Modal show={showModal} onHide={() => setShowModal(false)} centered className="ppc-modal transcript-form">
        <Modal.Header closeButton>
          <h2>Import from Transcript</h2>
        </Modal.Header>
        <Modal.Body>
          <Form className="ppc-modal-form">
            <Form.Group>
              <p>
                <b>Warning:</b> This will overwrite your current planner data.
              </p>
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
                }}
              ></Form.Control>
            </Form.Group>
          </Form>
          <Button variant="primary" disabled={!file || busy} onClick={importHandler}>
            {busy ? 'Importing...' : 'Import'}
          </Button>
        </Modal.Body>
      </Modal>
      <Button
        variant={darkMode ? 'dark' : 'light'}
        className="ppc-btn add-transcript-btn"
        onClick={() => setShowModal(true)}
      >
        <FileEarmarkText className="add-transcript-icon" />
        <div>Import Transcript</div>
      </Button>
    </>
  );
};

export default ImportTranscriptPopup;
