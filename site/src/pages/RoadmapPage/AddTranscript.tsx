import { FC, useContext, useState } from 'react';
import { FileEarmarkText } from 'react-bootstrap-icons';
import { Button, Form, Modal } from 'react-bootstrap';
import { setTransfers, setYearPlans } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import { parse as parseHTML, HTMLElement } from 'node-html-parser';
import ThemeContext from '../../style/theme-context';
import { CourseGQLData, PlannerQuarterData, PlannerYearData, QuarterName } from '../../types/types';
import { transformQuarterName } from '../../helpers/planner';

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

function processRow(row: HTMLElement, transfers: TransferUnitDetails[], quarters: TranscriptQuarter[]): void {
  const text = row.text.trim();
  if (!text) return;

  if (row.classList.contains('title')) {
    quarters.push({ name: text, courses: [] });
    return;
  }

  // No quarters at UCI Yet; these are transferred credits
  if (!quarters.length) {
    if (text.startsWith('Units Transferred')) return;
    const [raw, name, score, units, date] = text.match(/([^(]+)\s\(Score\s(\d),\sUnits ([\d.]+)\)\s([\d/]+)/) || [];
    if (!raw) return;
    transfers.push({ raw, name, score: Number(score), units: Number(units), date });
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

type CourseLookup = { [k: string]: CourseGQLData };

async function transcriptCourseDetails(quarters: TranscriptQuarter[]): Promise<CourseLookup> {
  const courseEntries = quarters
    .flatMap((q) => q.courses)
    .map((c, i) => {
      const dept = JSON.stringify(c.dept);
      const cNum = JSON.stringify(c.code);
      return `_${i}: courses(department: ${dept}, courseNumber: ${cNum}) { ...Details }`;
    });

  const query = `fragment Details on Course {
    id, department, courseNumber, school, title, courseLevel,
    minUnits, maxUnits, description, departmentName,
    instructors { ucinetid, name, shortenedName },
    prerequisiteTree, prerequisiteText,
    prerequisites { id, department, courseNumber, title },
    dependencies { id, department, courseNumber, title },
    repeatability, concurrent, sameAs, restriction, overlap,
    corequisites, geList, geText, terms
  }
  query CourseDetails { ${courseEntries.join(',\n')} }`;

  const response = await fetch('https://api-next.peterportal.org/v1/graphql', {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ query }),
  }).then((x) => x.json());

  // Query Response is { data: { [key: string]: [CourseData] } }
  // Transform this response into a lookup { courseID: {CourseData} }
  return Object.fromEntries(
    Object.values<CourseGQLData[]>(response.data)
      .flat()
      .map((c) => [c.department + c.courseNumber, c]),
  );
}

function toPlannerQuarter(
  quarter: TranscriptQuarter,
  courses: CourseLookup,
): { startYear: number; quarterData: PlannerQuarterData } {
  const year = parseInt(quarter.name.split(' ')[0]);
  // Removes the year number and "Session/Quarter" at the end
  const tName = quarter.name.split(' ').slice(1, -1).join(' ') as QuarterName;
  const name = transformQuarterName(tName);

  return {
    startYear: name === 'Fall' ? year : year - 1,
    quarterData: { name, courses: quarter.courses.map((c) => courses[c.dept + c.code]) },
  };
}

function groupIntoYears(quarters: { startYear: number; quarterData: PlannerQuarterData }[]) {
  return quarters.reduce(
    (years, q) => {
      const yearIdx = Object.keys(years).length + 1;

      if (years[q.startYear]) years[q.startYear].quarters.push(q.quarterData);
      else years[q.startYear] = { startYear: q.startYear, name: 'Year ' + yearIdx, quarters: [q.quarterData] };

      return years;
    },
    {} as { [k: string]: PlannerYearData },
  );
}

async function processTranscript(file: Blob) {
  const doc = await htmlFromFile(file);

  const rows = [...doc.querySelectorAll('#chrono-view .lineItems tr')];
  const transfers: TransferUnitDetails[] = [];
  const quarters: TranscriptQuarter[] = [];

  // Process the rows in the transcript and create a course lookup by code
  rows.forEach((r) => processRow(r, transfers, quarters));
  const courses = await transcriptCourseDetails(quarters);

  // Create the planner quarter format (with course details) by using the
  // course lookup and the grouped quarters in the transcript
  const plannerQuarters = quarters.map((q) => toPlannerQuarter(q, courses));
  plannerQuarters.sort((a, b) => a.startYear - b.startYear);

  const years = groupIntoYears(plannerQuarters);
  return { transfers, years };
}

const AddTranscript: FC = () => {
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
      dispatch(setTransfers(transfers)); // these types are compatible
      dispatch(setYearPlans(Object.values(years)));
      setShowModal(false);
      setFile(null);
    } finally {
      setBusy(false);
    }
  };

  return (
    <>
      <Modal show={showModal} onHide={() => setShowModal(false)} centered className="planner-year-modal">
        <Modal.Header closeButton>
          <h2>Import from Transcript</h2>
        </Modal.Header>
        <Modal.Body>
          <Form className="add-year-form">
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
              <Form.Label className="add-year-form-label">Transcript File</Form.Label>
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
      <Button variant={darkMode ? 'dark' : 'light'} onClick={() => setShowModal(true)}>
        <FileEarmarkText className="add-year-icon" />
        <div className="add-year-text">Import Transcript</div>
      </Button>
    </>
  );
};

export default AddTranscript;
