import { FC, useState, useEffect, useCallback, useContext } from 'react';
import { Dropdown, DropdownButton } from 'react-bootstrap';
import './GradeDist.scss';

import Chart from './Chart';
import Pie from './Pie';

import { CourseGQLData, ProfessorGQLData, DataType } from '../../types/types';
import { GradesRaw, QuarterName } from '@peterportal/types';
import trpc from '../../trpc';
import ThemeContext from '../../style/theme-context';

interface GradeDistProps {
  dataType: DataType;
  data: CourseGQLData | ProfessorGQLData;
  minify?: boolean;
}

interface Entry {
  value: string;
  text: string;
}

type ChartTypes = 'bar' | 'pie';

const quarterOrder: QuarterName[] = ['Winter', 'Spring', 'Summer1', 'Summer10wk', 'Summer2', 'Fall'];

const GradeDist: FC<GradeDistProps> = ({ dataType, data, minify }) => {
  const [gradeDistData, setGradeDistData] = useState<GradesRaw>([]);
  const [loading, setLoading] = useState(false);
  const [chartType, setChartType] = useState<ChartTypes>('bar');
  const [currentQuarter, setCurrentQuarter] = useState('');
  const [quarterEntries, setQuarterEntries] = useState<Entry[]>([]);
  const [currentData, setCurrentData] = useState('');
  const [dataEntries, setDataEntries] = useState<Entry[]>([]);
  const { darkMode } = useContext(ThemeContext);
  const buttonVariant = darkMode ? 'dark' : 'light';

  const fetchGradeDistData = useCallback(async () => {
    setGradeDistData([]);
    setLoading(true);

    let requests: Promise<GradesRaw>[];

    if (dataType === 'course') {
      const { department, courseNumber } = data as CourseGQLData;
      requests = [trpc.courses.grades.query({ department, number: courseNumber })];
    } else {
      const professorData = data as ProfessorGQLData;
      requests = professorData.shortenedNames.map((name) => trpc.professors.grades.query({ name }));
    }

    try {
      const res = await Promise.all(requests);
      setGradeDistData(res.flat());
    } finally {
      setLoading(false);
    }
  }, [dataType, data]);

  useEffect(() => {
    fetchGradeDistData();
  }, [fetchGradeDistData]);

  // Create an array of objects to feed into the course/prof dropdown menu
  const createDataEntries = useCallback(() => {
    const entries =
      dataType === 'course'
        ? gradeDistData.flatMap((match) => match.instructors)
        : gradeDistData.map((match) => `${match.department} ${match.courseNumber}`);

    const dataEntries: Entry[] = Array.from(new Set(entries))
      .sort((a, b) => a.localeCompare(b))
      .map((entry) => ({
        value: entry,
        text: entry,
      }));

    setDataEntries(dataEntries);
    setCurrentData(dataEntries[0]?.value ?? '');
  }, [gradeDistData, dataType]);

  // update list of professors/courses when new course/professor is detected
  useEffect(() => {
    if (!gradeDistData.length) return;
    createDataEntries();
  }, [gradeDistData, createDataEntries]);

  // Create an array of objects to feed into the quarter dropdown menu.
  const createQuarterEntries = useCallback(() => {
    const quarters = gradeDistData
      .filter((entry) =>
        dataType === 'course'
          ? entry.instructors.includes(currentData)
          : entry.department + ' ' + entry.courseNumber === currentData,
      )
      .map((entry) => `${entry.quarter} ${entry.year}`);

    const result: Entry[] = [{ value: 'ALL', text: 'All Quarters' }]
      .concat(Array.from(new Set(quarters)).map((q) => ({ value: q, text: q })))
      .sort((a, b) => {
        if (a.value === 'ALL') return -1;
        if (b.value === 'ALL') return 1;

        const [quarterA, yearA] = a.value.split(' ') as [QuarterName, string];
        const [quarterB, yearB] = b.value.split(' ') as [QuarterName, string];

        return yearA === yearB
          ? quarterOrder.indexOf(quarterB) - quarterOrder.indexOf(quarterA)
          : Number.parseInt(yearB) - Number.parseInt(yearA);
      });

    setQuarterEntries(result);
    setCurrentQuarter(result[0].value);
  }, [gradeDistData, currentData, dataType]);

  // update list of quarters when new professor/course is chosen
  useEffect(() => {
    if (!gradeDistData.length || !currentData) return;
    createQuarterEntries();
  }, [gradeDistData, currentData, createQuarterEntries]);

  const ChartTypeDropdown = () => {
    return (
      <div className="gradedist-filter">
        <DropdownButton
          className="ppc-dropdown-btn"
          title="Chart Type"
          variant={buttonVariant}
          onSelect={(value) => setChartType(value as ChartTypes)}
        >
          <Dropdown.Item eventKey="bar">Bar</Dropdown.Item>
          <Dropdown.Item eventKey="pie">Pie</Dropdown.Item>
        </DropdownButton>
      </div>
    );
  };

  const DataOptionsDropdown = () => {
    return (
      <div className="gradedist-filter">
        <DropdownButton
          className="ppc-dropdown-btn"
          title={currentData}
          variant={buttonVariant}
          onSelect={(value) => setCurrentData(value ?? '')}
        >
          {dataEntries.map((entry) => (
            <Dropdown.Item key={entry.value} eventKey={entry.value}>
              {entry.text}
            </Dropdown.Item>
          ))}
        </DropdownButton>
      </div>
    );
  };

  const QuarterOptionsDropdown = () => {
    return (
      <div className="gradedist-filter">
        <DropdownButton
          className="ppc-dropdown-btn"
          title={quarterEntries.find((q) => q.value === currentQuarter)?.text ?? 'Quarter'}
          variant={buttonVariant}
          onSelect={(value) => setCurrentQuarter(value ?? '')}
        >
          {quarterEntries.map((entry) => (
            <Dropdown.Item key={entry.value} eventKey={entry.value}>
              {entry.text}
            </Dropdown.Item>
          ))}
        </DropdownButton>
      </div>
    );
  };

  const graphProps = {
    gradeData: gradeDistData,
    quarter: currentQuarter,
    data: currentData,
    dataType,
  };

  return (
    <div className={`gradedist-module-container ${minify && 'grade-dist-mini'}`}>
      <div className="gradedist-menu">
        {minify && <ChartTypeDropdown />}
        <DataOptionsDropdown />
        <QuarterOptionsDropdown />
      </div>
      {!gradeDistData.length ? (
        <div className="chart-container">
          <div className={`grade_distribution_chart-container ${chartType}`}>
            {chartType === 'bar' ? <Chart {...graphProps} /> : <Pie {...graphProps} />}
          </div>
        </div>
      ) : (
        <div style={{ height: 400, textAlign: 'center' }}>
          <p>{loading ? 'Loading Distribution...' : 'Error: could not retrieve grade distribution data.'}</p>
        </div>
      )}
    </div>
  );
};

export default GradeDist;
