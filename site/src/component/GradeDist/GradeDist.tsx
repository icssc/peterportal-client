import React, { FC, useState, useEffect } from 'react';
import { Divider, Dropdown, Grid, DropdownProps } from 'semantic-ui-react';
import Chart from './Chart';
import Pie from './Pie';
import './GradeDist.scss'

import { CourseData, GradeDistData } from '../../types/types';

interface GradeDistProps extends CourseData {
  currentProf?: string
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
  const [currentProf, setCurrentProf] = useState(props.currentProf || '');
  const [profEntries, setProfEntries] = useState<Entry[]>(null!);
  const [quarterEntries, setQuarterEntries] = useState<Entry[]>(null!);

  // initial request to get grade dist data
  useEffect(() => {
    if (gradeDistData == null) {
      fetch(`/courses/api/grades/${props.department}/${props.number}`)
        .then(response => response.json())
        .then(data => {
          setGradeDistData(data);
        });
    }
  })

  // update list of professors when new course is detected
  useEffect(() => {
    if (gradeDistData && gradeDistData.length !== 0) {
      createProfEntries();
    }
  }, [gradeDistData])

  // update list of quarters when new professor is chosen
  useEffect(() => {
    if (currentProf && gradeDistData.length !== 0) {
      createQuarterEntries();
    }
  }, [currentProf])

  /*
   * Create an array of objects to feed into the quarter dropdown menu.
   * @return an array of JSON objects recording each quarter
   */
  const createQuarterEntries = () => {
    let quarters: Set<string> = new Set()
    let result: Entry[] = [];

    gradeDistData
      .filter(entry => entry.instructor === currentProf)
      .forEach(data => quarters.add(data.quarter + ' ' + data.year));
    quarters.forEach(quarter => result.push({ value: quarter, text: quarter }));

    console.log(currentProf, quarters, result)
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

  if (gradeDistData !== null && gradeDistData.length !== 0) {
    return (
      <div id='gradedist-module-container'>
        <Grid.Row columns={2} id='menu'>
          <Grid.Column style={{ marginRight: '1rem' }}>
            <Dropdown
              placeholder='Professor'
              scrolling
              selection
              options={profEntries}
              value={currentProf}
              onChange={updateCurrentProf}
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
            <Chart
              gradeData={gradeDistData}
              quarter={currentQuarter}
              professor={currentProf}
            />
          </div>
          <div className={'grade_distribution_chart-container pie'}>
            <Pie
              gradeData={gradeDistData}
              quarter={currentQuarter}
              professor={currentProf}
            />
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