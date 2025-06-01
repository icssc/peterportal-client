import React from 'react';
import { ResponsiveBar, BarTooltipProps, BarDatum } from '@nivo/bar';

import ThemeContext from '../../style/theme-context';
import { type Theme } from '@nivo/core';
import { GradesRaw } from '@peterportal/types';
import { GradeColors } from './gradeColors.ts';
import { tooltipStyle } from './tooltipStyle.ts';

interface ChartProps {
  gradeData: GradesRaw;
  quarter: string;
  professor?: string;
  course?: string;
}

export default class Chart extends React.Component<ChartProps> {
  /*
   * Initialize the grade distribution chart on the webpage.
   */

  getTheme = (darkMode: boolean): Theme => {
    return {
      axis: {
        ticks: {
          text: {
            fill: darkMode ? '#eee' : '#333',
          },
        },
        legend: {
          text: {
            fill: darkMode ? '#eee' : '#333',
          },
        },
      },
    };
  };

  /*
   * Create an array of objects to feed into the chart.
   * @return an array of JSON objects detailing the grades for each class
   */
  getClassData = (): BarDatum[] => {
    let gradeACount = 0,
      gradeBCount = 0,
      gradeCCount = 0,
      gradeDCount = 0,
      gradeFCount = 0,
      gradePCount = 0,
      gradeNPCount = 0;

    this.props.gradeData.forEach((data) => {
      if (
        ((data.quarter + ' ' + data.year === this.props.quarter || this.props.quarter == 'ALL') &&
          data.instructors.includes(this.props.professor ?? '')) ||
        data.department + ' ' + data.courseNumber === this.props.course ||
        this.props.professor == 'All'
      ) {
        gradeACount += data.gradeACount;
        gradeBCount += data.gradeBCount;
        gradeCCount += data.gradeCCount;
        gradeDCount += data.gradeDCount;
        gradeFCount += data.gradeFCount;
        gradePCount += data.gradePCount;
        gradeNPCount += data.gradeNPCount;
      }
    });

    return [
      {
        id: 'A',
        label: 'A',
        A: gradeACount,
        color: GradeColors.A,
      },
      {
        id: 'B',
        label: 'B',
        B: gradeBCount,
        color: GradeColors.B,
      },
      {
        id: 'C',
        label: 'C',
        C: gradeCCount,
        color: GradeColors.C,
      },
      {
        id: 'D',
        label: 'D',
        D: gradeDCount,
        color: GradeColors.D,
      },
      {
        id: 'F',
        label: 'F',
        F: gradeFCount,
        color: GradeColors.F,
      },
      {
        id: 'P',
        label: 'P',
        P: gradePCount,
        color: GradeColors.P,
      },
      {
        id: 'NP',
        label: 'NP',
        NP: gradeNPCount,
        color: GradeColors.NP,
      },
    ];
  };

  /*
   * Indicate how the tooltip should look like when users hover over the bar
   * Code is slightly modified from: https://codesandbox.io/s/nivo-scatterplot-
   * vs-bar-custom-tooltip-7u6qg?file=/src/index.js:1193-1265
   * @param event an event object recording the mouse movement, etc.
   * @return a JSX block styling the chart
   */
  styleTooltip = (props: BarTooltipProps<BarDatum>) => {
    return (
      <div style={tooltipStyle.tooltip?.container}>
        <strong>
          {props.label}: {props.data[props.label]}
        </strong>
      </div>
    );
  };

  /*
   * Display the grade distribution chart.
   * @return a JSX block rendering the chart
   */
  render() {
    const data = this.getClassData();

    // greatestCount calculates the upper bound of the graph (i.e. the greatest number of students in a single grade)
    const greatestCount = data.reduce(
      (max, grade) => ((grade[grade.id] as number) > max ? (grade[grade.id] as number) : max),
      0,
    );

    // The base marginX is 30, with increments of 5 added on for every order of magnitude greater than 100 to accomadate for larger axis labels (1,000, 10,000, etc)
    // For example, if greatestCount is 5173 it is (when rounding down (i.e. floor)), one magnitude (calculated with log_10) greater than 100, therefore we add one increment of 5px to our base marginX of 30px
    // Math.max() ensures that we're not finding the log of a non-positive number
    const marginX = 30 + 5 * Math.floor(Math.log10(Math.max(100, greatestCount) / 100));

    return (
      <>
        <ThemeContext.Consumer>
          {({ darkMode }) => (
            <ResponsiveBar
              data={data}
              keys={['A', 'B', 'C', 'D', 'F', 'P', 'NP']}
              indexBy="label"
              margin={{
                top: 50,
                right: marginX,
                bottom: 50,
                left: marginX,
              }}
              layout="vertical"
              axisBottom={{
                tickSize: 10,
                tickPadding: 5,
                tickRotation: 0,
                legend: 'Grade',
                legendPosition: 'middle',
                legendOffset: 36,
              }}
              enableLabel={false}
              colors={Object.values(GradeColors)}
              theme={this.getTheme(darkMode)}
              tooltipLabel={(datum) => String(datum.id)}
              tooltip={this.styleTooltip}
            />
          )}
        </ThemeContext.Consumer>
      </>
    );
  }
}
