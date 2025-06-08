import { Component } from 'react';
import { ResponsiveBar, BarTooltipProps, BarDatum } from '@nivo/bar';

import ThemeContext from '../../style/theme-context';
import { type Theme } from '@nivo/core';
import { GradesRaw, letterGrades } from '@peterportal/types';
import { DataType } from '../../types/types';
import { GradeColors } from './gradeColors.ts';
import { tooltipStyle } from './tooltipStyle.ts';

interface ChartProps {
  gradeData: GradesRaw;
  quarter: string;
  data: string;
  dataType: DataType;
}

export default class Chart extends Component<ChartProps> {
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

  getGradeData = (): BarDatum[] => {
    const { gradeData, dataType, data, quarter } = this.props;
    const gradeCounts = { A: 0, B: 0, C: 0, D: 0, F: 0, P: 0, NP: 0 };

    gradeData.forEach((entry) => {
      const correctQuarter = quarter === 'ALL' || `${entry.quarter} ${entry.year}` === quarter;
      const correctData =
        dataType === 'professor'
          ? entry.instructors.includes(data)
          : `${entry.department} ${entry.courseNumber}` === data;

      if (correctQuarter && correctData) {
        letterGrades.forEach((grade) => {
          gradeCounts[grade] += entry[`grade${grade}Count` as keyof typeof entry] as number;
        });
      }
    });

    return letterGrades.map((grade) => ({
      id: grade,
      label: grade,
      [grade]: gradeCounts[grade],
      color: GradeColors[grade],
    }));
  };

  styleTooltip = (props: BarTooltipProps<BarDatum>) => {
    return (
      <div style={tooltipStyle.tooltip?.container}>
        <strong>
          {props.label}: {props.data[props.label]}
        </strong>
      </div>
    );
  };

  render() {
    const data = this.getGradeData();

    // calculates the graph's upper bound, aka the greatest number of students in a single grade
    const greatestCount = data.reduce(
      (max, grade) => ((grade[grade.id] as number) > max ? (grade[grade.id] as number) : max),
      0,
    );

    // The base marginX is 30, with increments of 5 added on for every order of magnitude
    // greater than 100 to accomadate for larger axis labels (1,000, 10,000, etc)
    // For example, if greatestCount is 5173 it is (when rounding down (i.e. floor)), one
    // magnitude (calculated with log_10) greater than 100, therefore we add one increment
    // of 5px to our base marginX of 30px
    // Math.max() ensures that we're not finding the log of a non-positive number
    const marginX = 30 + 5 * Math.floor(Math.log10(Math.max(100, greatestCount) / 100));

    return (
      <ThemeContext.Consumer>
        {({ darkMode }) => (
          <ResponsiveBar
            data={data}
            keys={letterGrades}
            colors={Object.values(GradeColors)}
            theme={this.getTheme(darkMode)}
            tooltip={this.styleTooltip}
            tooltipLabel={(datum) => String(datum.id)}
            enableLabel={false}
            indexBy="label"
            layout="vertical"
            margin={{
              top: 50,
              right: marginX,
              bottom: 50,
              left: marginX,
            }}
            axisBottom={{
              tickSize: 10,
              tickPadding: 5,
              tickRotation: 0,
              legend: 'Grade',
              legendPosition: 'middle',
              legendOffset: 36,
            }}
          />
        )}
      </ThemeContext.Consumer>
    );
  }
}
