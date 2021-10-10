import React, { FC, useState, useEffect } from 'react';
import { Divider, Dropdown, Grid, DropdownProps } from 'semantic-ui-react';
import Chart from './Chart';
import Pie from './Pie';
import './GradeDist.scss'

import { CourseData, ProfessorData, GradeDistData } from '../../types/types';

interface GradeDistProps {
  course?: CourseData;
  professor?: ProfessorData;
}

interface Entry {
  value: string;
  text: string;
}

const GradeDist: FC<GradeDistProps> = (props) => {
  /*
 * Initialize a GradeDist block on the webpage.
 * @param props attributes received from the parent element
 */

  const [gradeDistData, setGradeDistData] = useState<GradeDistData>(null!);
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
      url = `/courses/api/grades`;
      params = {
        department: props.course.department,
        number: props.course.number
      }
    }
    else if (props.professor) {
      url = `/professors/api/grades/${props.professor.shortened_name}`;
    }
    fetch(url)
      .then(response => response.json())
      .then(data => {
        console.log(data);
        setGradeDistData(data);
      });
  }

  // initial request to get grade dist data
  useEffect(() => {
    if (gradeDistData == null) {
      fetchGradeDistData();
    }
  })

  // get new data if choose a new course or professor
  useEffect(() => {
    setGradeDistData([]);
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
        if (props.course && entry.instructor === currentProf) {
          return true;
        }
        if (props.professor && (entry.department + ' ' + entry.number) == currentCourse) {
          return true;
        }
        return false;
      })
      .forEach(data => quarters.add(data.quarter + ' ' + data.year));
    quarters.forEach(quarter => result.push({ value: quarter, text: quarter }));

    setQuarterEntries(result);
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
      .forEach(match => professors.add(match.instructor));

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
      .forEach(match => courses.add(match.department + ' ' + match.number));

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
      <div id='gradedist-module-container'>
        <Grid.Row columns={2} id='menu'>
          <Grid.Column style={{ marginRight: '1rem' }}>
            <Dropdown
              placeholder={props.course ? 'Professor' : 'Course'}
              scrolling
              selection
              options={props.course ? profEntries : courseEntries}
              value={props.course ? currentProf : currentCourse}
              onChange={props.course ? updateCurrentProf : updateCurrentCourse}
            />
          </Grid.Column>

          <Grid.Column>
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
          <div className={'grade_distribution_chart-container chart'}>
            <Chart {...graphProps} />
          </div>
          <div className={'grade_distribution_chart-container pie'}>
            <Pie {...graphProps} />
          </div>
        </Grid.Row>
      </div>
    );
  } else {
    return (
      <div>
      </div>
    );
  }
}

export default GradeDist;