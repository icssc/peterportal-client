import { FC, useState, useEffect, useCallback, useContext } from 'react';
import Chart from './Chart';
import Pie from './Pie';
import './GradeDist.scss';

import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import { GradesRaw, QuarterName } from '@peterportal/types';
import trpc from '../../trpc';
import { Dropdown, DropdownButton } from 'react-bootstrap';
import ThemeContext from '../../style/theme-context';

interface GradeDistProps {
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
  minify?: boolean;
}

interface Entry {
  value: string;
  text: string;
}

type ChartTypes = 'bar' | 'pie';

const quarterOrder: QuarterName[] = ['Winter', 'Spring', 'Summer1', 'Summer10wk', 'Summer2', 'Fall'];

const GradeDist: FC<GradeDistProps> = (props) => {
  /*
   * Initialize a GradeDist block on the webpage.
   * @param props attributes received from the parent element
   */

  const [gradeDistData, setGradeDistData] = useState<GradesRaw>();
  const [chartType, setChartType] = useState<ChartTypes>('bar');
  const [currentQuarter, setCurrentQuarter] = useState('');
  const [currentProf, setCurrentProf] = useState('');
  const [profEntries, setProfEntries] = useState<Entry[]>();
  const [currentCourse, setCurrentCourse] = useState('');
  const [courseEntries, setCourseEntries] = useState<Entry[]>();
  const [quarterEntries, setQuarterEntries] = useState<Entry[]>();
  const { darkMode } = useContext(ThemeContext);
  const buttonVariant = darkMode ? 'dark' : 'light';

  const fetchGradeDistData = useCallback(() => {
    let requests: Promise<GradesRaw>[];
    // course context
    if (props.course) {
      const params = {
        department: props.course.department,
        number: props.course.courseNumber,
      };
      requests = [trpc.courses.grades.query(params)];
    } else if (props.professor) {
      requests = props.professor.shortenedNames.map((name) => trpc.professors.grades.query({ name }));
    }

    Promise.all(requests!)
      .then((res) => res.flat())
      .then(setGradeDistData)
      .catch((error) => {
        setGradeDistData([]);
        console.error(error.response);
      });
  }, [props.course, props.professor]);

  // reset any data from a previous course or professor, get new data for course or professor
  useEffect(() => {
    setGradeDistData(null!);
    fetchGradeDistData();
  }, [fetchGradeDistData]);

  /*
   * Create an array of objects to feed into the professor dropdown menu.
   * @return an array of JSON objects recording professor's names
   */
  const createProfEntries = useCallback(() => {
    const professors: Set<string> = new Set();
    const result: Entry[] = [{ value: 'ALL', text: 'All Instructors' }];

    gradeDistData!.forEach((match) => match.instructors.forEach((prof) => professors.add(prof)));

    Array.from(professors)
      .sort((a, b) => a.localeCompare(b))
      .forEach((professor) => result.push({ value: professor, text: professor }));

    setProfEntries(result);
    setCurrentProf(result[0].value);
  }, [gradeDistData]);

  /*
   * Create an array of objects to feed into the course dropdown menu.
   * @return an array of JSON objects recording course's names
   */
  const createCourseEntries = useCallback(() => {
    const courses: Set<string> = new Set();
    const result: Entry[] = [];

    gradeDistData!.forEach((match) => courses.add(match.department + ' ' + match.courseNumber));

    Array.from(courses)
      .sort((a, b) => a.localeCompare(b))
      .forEach((course) => result.push({ value: course, text: course }));

    setCourseEntries(result);
    setCurrentCourse(result[0].text);
  }, [gradeDistData]);

  // update list of professors/courses when new course/professor is detected
  useEffect(() => {
    if (gradeDistData?.length) {
      if (props.course) {
        createProfEntries();
      } else if (props.professor) {
        createCourseEntries();
      }
    }
  }, [gradeDistData, createCourseEntries, createProfEntries, props.course, props.professor]);

  /*
   * Create an array of objects to feed into the quarter dropdown menu.
   * @return an array of JSON objects recording each quarter
   */
  const createQuarterEntries = useCallback(() => {
    const quarters: Set<string> = new Set();
    const result: Entry[] = [{ value: 'ALL', text: 'All Quarters' }];

    gradeDistData!
      .filter((entry) => {
        if (
          props.course &&
          currentProf === 'ALL' &&
          entry.department + ' ' + entry.courseNumber === props.course.department + ' ' + props.course.courseNumber
        ) {
          return true;
        }

        if (props.course && entry.instructors.includes(currentProf)) {
          return true;
        }
        if (props.professor && entry.department + ' ' + entry.courseNumber == currentCourse) {
          return true;
        }
        return false;
      })
      .forEach((data) => quarters.add(data.quarter + ' ' + data.year));
    quarters.forEach((quarter) => result.push({ value: quarter, text: quarter }));

    setQuarterEntries(
      result.sort((a, b) => {
        if (a.value === 'ALL') {
          return -1;
        }
        if (b.value === 'ALL') {
          return 1;
        }
        const [thisQuarter, thisYear] = a.value.split(' ') as [QuarterName, string];
        const [thatQuarter, thatYear] = b.value.split(' ') as [QuarterName, string];
        if (thisYear === thatYear) {
          return quarterOrder.indexOf(thatQuarter) - quarterOrder.indexOf(thisQuarter);
        } else {
          return Number.parseInt(thatYear, 10) - Number.parseInt(thisYear, 10);
        }
      }),
    );
    setCurrentQuarter(result[0].value);
  }, [currentCourse, currentProf, gradeDistData, props.course, props.professor]);

  // update list of quarters when new professor/course is chosen
  useEffect(() => {
    if ((currentProf || currentCourse) && gradeDistData?.length) {
      createQuarterEntries();
    }
  }, [currentProf, currentCourse, createQuarterEntries, gradeDistData]);

  const profCourseOptions = props.course ? profEntries : courseEntries;
  const profCourseSelectedValue = props.course ? currentProf : currentCourse;
  const updateProfCourse = (value: string | null) => {
    if (props.course) setCurrentProf(value!);
    else setCurrentCourse(value!);
  };

  const selectedQuarterName = quarterEntries?.find((q) => q.value === currentQuarter)?.text ?? 'Quarter';
  const selectedProfCourseName =
    profCourseOptions?.find((p) => p.value === profCourseSelectedValue)?.text ?? 'Professor';

  const optionsRow = (
    <div className="gradedist-menu">
      {props.minify && (
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
      )}

      <div className="gradedist-filter">
        <DropdownButton
          className="ppc-dropdown-btn"
          title={selectedProfCourseName}
          variant={buttonVariant}
          onSelect={updateProfCourse}
        >
          {profCourseOptions?.map((q) => {
            return (
              <Dropdown.Item key={q.value} eventKey={q.value}>
                {q.text}
              </Dropdown.Item>
            );
          })}
        </DropdownButton>
      </div>

      <div className="gradedist-filter">
        <DropdownButton
          className="ppc-dropdown-btn"
          title={selectedQuarterName}
          variant={buttonVariant}
          onSelect={(value) => setCurrentQuarter(value!)}
        >
          {quarterEntries?.map((q) => {
            return (
              <Dropdown.Item key={q.value} eventKey={q.value}>
                {q.text}
              </Dropdown.Item>
            );
          })}
        </DropdownButton>
      </div>
    </div>
  );

  if (gradeDistData?.length) {
    const graphProps = {
      gradeData: gradeDistData,
      quarter: currentQuarter,
      course: currentCourse,
      professor: currentProf,
    };
    return (
      <div className={`gradedist-module-container ${props.minify ? 'grade-dist-mini' : ''}`}>
        {optionsRow}
        <div className="chart-container">
          {((props.minify && chartType == 'bar') || !props.minify) && (
            <div className={'grade_distribution_chart-container chart'}>
              <Chart {...graphProps} />
            </div>
          )}
          {((props.minify && chartType == 'pie') || !props.minify) && (
            <div className={'grade_distribution_chart-container pie'}>
              <Pie {...graphProps} />
            </div>
          )}
        </div>
      </div>
    );
  } else if (gradeDistData == null) {
    // null if still fetching, display loading message
    return (
      <div className={`gradedist-module-container ${props.minify ? 'grade-dist-mini' : ''}`}>
        {optionsRow}
        <div style={{ height: 400, textAlign: 'center' }}>
          <p>Loading Distribution..</p>
        </div>
      </div>
    );
  } else {
    // gradeDistData is empty, did not receive any data from API call or received an error, display an error message
    return (
      <div className={`gradedist-module-container ${props.minify ? 'grade-dist-mini' : ''}`}>
        {optionsRow}
        <div style={{ height: 400, textAlign: 'center' }}>
          <p>Error: could not retrieve grade distribution data.</p>
        </div>
      </div>
    );
  }
};

export default GradeDist;
