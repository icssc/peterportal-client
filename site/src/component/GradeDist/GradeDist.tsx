import React, { FC, useState, useEffect } from 'react';
import { Divider, Dropdown, Grid, DropdownProps } from 'semantic-ui-react';
import Chart from './Chart';
import Pie from './Pie';
import './GradeDist.scss'
import axios from 'axios'

import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import { GradesRaw } from "peterportal-api-next-types";

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

const GradeDist: FC<GradeDistProps> = (props) => {
  const quarterOrder = ["Winter", "Spring", "Summer1", "Summer10wk", "Summer2", "Fall"]
  /*
 * Initialize a GradeDist block on the webpage.
 * @param props attributes received from the parent element
 */

  const [gradeDistData, setGradeDistData] = useState<GradesRaw>(null!);
  const [chartType, setChartType] = useState<ChartTypes>('bar');
  const [currentQuarter, setCurrentQuarter] = useState('');
  const [currentProf, setCurrentProf] = useState('');
  const [profEntries, setProfEntries] = useState<Entry[]>(null!);
  const [currentCourse, setCurrentCourse] = useState('');
  const [courseEntries, setCourseEntries] = useState<Entry[]>(null!);
  const [quarterEntries, setQuarterEntries] = useState<Entry[]>(null!);

  const fetchGradeDistData = () => {
    let url = '';
    let params = {};
    // course context
    if (props.course) {
      url = `/api/courses/api/grades`;
      params = {
        department: props.course.department.replace(/ /g, ''),
        number: props.course.courseNumber
      }
    }
    else if (props.professor) {
      url = `/api/professors/api/grades/${props.professor.shortenedName}`;
    }
    const res = axios.get<GradesRaw>(url, {
      params: params
    })
      .then(res => {
        console.log(res)
        setGradeDistData(res.data);
      }).catch(error => {
        setGradeDistData([]);
        console.error(error.response);
      });
  }

  // reset any data from a previous course or professor, get new data for course or professor
  useEffect(() => {
    setGradeDistData(null!);
    fetchGradeDistData();
  }, [props.course?.id, props.professor?.ucinetid])

  // update list of professors/courses when new course/professor is detected
  useEffect(() => {
    if (gradeDistData && gradeDistData.length !== 0) {
      if (props.course) {
        createProfEntries();
      }
      else if (props.professor) {
        createCourseEntries();
      }
    }
  }, [gradeDistData])

  // update list of quarters when new professor/course is chosen
  useEffect(() => {
    if ((currentProf || currentCourse) && gradeDistData.length !== 0) {
      createQuarterEntries();
    }
  }, [currentProf, currentCourse])

  /*
   * Create an array of objects to feed into the quarter dropdown menu.
   * @return an array of JSON objects recording each quarter
   */
  const createQuarterEntries = () => {
    let quarters: Set<string> = new Set()
    let result: Entry[] = [{ value: 'ALL', text: 'All Quarters' }];

    gradeDistData
      .filter(entry => {
        if (props.course && entry.instructors.includes(currentProf)) {
          return true;
        }
        if (props.professor && (entry.department + ' ' + entry.courseNumber) == currentCourse) {
          return true;
        }
        return false;
      })
      .forEach(data => quarters.add(data.quarter + ' ' + data.year));
    quarters.forEach(quarter => result.push({ value: quarter, text: quarter }));

    setQuarterEntries(result.sort((a, b) => {
      if (a.value === "ALL") {
        return -1;
      }
      if (b.value === "ALL") {
        return 1;
      }
      const [thisQuarter, thisYear] = a.value.split(" ");
      const [thatQuarter, thatYear] = b.value.split(" ");
      if (thisYear === thatYear) {
        return quarterOrder.indexOf(thatQuarter) - quarterOrder.indexOf(thisQuarter);
      } else {
        return Number.parseInt(thatYear, 10) - Number.parseInt(thisYear, 10);
      }
    }));
    setCurrentQuarter(result[0].value);
  }

  /*
   * Create an array of objects to feed into the professor dropdown menu.
   * @return an array of JSON objects recording professor's names
   */
  const createProfEntries = () => {
    let professors: Set<string> = new Set()
    let result: Entry[] = [];

    gradeDistData
      .forEach(match => match.instructors.forEach((prof) => professors.add(prof)));

    professors.forEach(professor => result.push(
      { value: professor, text: professor }
    ));

    setProfEntries(result);
    setCurrentProf(result[0].value);
  }

  /*
 * Create an array of objects to feed into the course dropdown menu.
 * @return an array of JSON objects recording course's names
 */
  const createCourseEntries = () => {
    let courses: Set<string> = new Set()
    let result: Entry[] = [];

    gradeDistData
      .forEach(match => courses.add(match.department + ' ' + match.courseNumber));

    courses.forEach(course => result.push(
      { value: course, text: course }
    ));

    setCourseEntries(result);
    setCurrentCourse(result[0].value);
  }

  /*
   * Record what is in the quarter dropdown menu at the moment.
   * @param event an event object recording the mouse movement, etc.
   * @param status details about the status in the dropdown menu
   */
  const updateCurrentQuarter = (event: React.SyntheticEvent<HTMLElement>, status: DropdownProps) => {
    setCurrentQuarter(status.value as string);
  }

  /*
   * Record what is in the professor dropdown menu at the moment.
   * @param event an event object recording the mouse movement, etc.
   * @param status details about the status in the dropdown menu
   */
  const updateCurrentProf = (event: React.SyntheticEvent<HTMLElement>, status: DropdownProps) => {
    setCurrentProf(status.value as string);
  }

  /*
 * Record what is in the course dropdown menu at the moment.
 * @param event an event object recording the mouse movement, etc.
 * @param status details about the status in the dropdown menu
 */
  const updateCurrentCourse = (event: React.SyntheticEvent<HTMLElement>, status: DropdownProps) => {
    setCurrentCourse(status.value as string);
  }

  if (gradeDistData !== null && gradeDistData.length !== 0) {
    let graphProps = {
      gradeData: gradeDistData,
      quarter: currentQuarter,
      course: currentCourse,
      professor: currentProf
    }
    return (
      <div className={`gradedist-module-container ${props.minify ? 'grade-dist-mini' : ''}`}>
        <Grid.Row id='menu'>
          {
            props.minify && <Grid.Column className='gradedist-filter'>
              <Dropdown
                placeholder='Chart Type'
                scrolling
                selection
                options={[{ text: 'Bar', value: 'bar' }, { text: 'Pie', value: 'pie' }]}
                value={chartType}
                onChange={(e, s) => setChartType(s.value as ChartTypes)}
              />
            </Grid.Column>
          }

          <Grid.Column className='gradedist-filter'>
            <Dropdown
              placeholder={props.course ? 'Professor' : 'Course'}
              scrolling
              selection
              options={props.course ? profEntries : courseEntries}
              value={props.course ? currentProf : currentCourse}
              onChange={props.course ? updateCurrentProf : updateCurrentCourse}
            />
          </Grid.Column>

          <Grid.Column className='gradedist-filter'>
            <Dropdown
              placeholder='Quarter'
              scrolling
              selection
              options={quarterEntries}
              value={currentQuarter}
              onChange={updateCurrentQuarter}
            />
          </Grid.Column>
        </Grid.Row>

        <Grid.Row id='chart'>
          {
            (props.minify && chartType == 'bar' || !props.minify) && <div className={'grade_distribution_chart-container chart'}>
              <Chart {...graphProps} />
            </div>
          }
          {
            (props.minify && chartType == 'pie' || !props.minify) && <div className={'grade_distribution_chart-container pie'}>
              <Pie {...graphProps} />
            </div>
          }
        </Grid.Row>
      </div>
    );
  } else if (gradeDistData == null) { // null if still fetching, display loading message
    return <>Loading Distribution..</>;
  } else { // gradeDistData is empty, did not receive any data from API call or received an error, display an error message
    return (
      <>
        Error: could not retrieve grade distribution data.
      </>
    );
  }
}

export default GradeDist;