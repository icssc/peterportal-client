import React from 'react';
import { ResponsiveBar, BarTooltipDatum } from '@nivo/bar'

import { GradeDistData } from '../../types/types';

const colors = { 'A': '#60A3D1', 'B': '#81C284', 'C': '#F5D77F', 'D': '#ECAD6D', 'F': '#E8966D', 'P': '#EBEBEB', 'NP': '#EBEBEB' }
const getColor = (bar: Bar) => colors[bar.id]

interface Bar {
  id: 'A' | 'B' | 'C' | 'D' | 'F' | 'P' | 'NP',
  label: string,
  color: string,
  A?: number,
  B?: number,
  C?: number,
  D?: number,
  F?: number,
  P?: number,
  NP?: number,
}

interface ChartProps {
  gradeData: GradeDistData;
  quarter: string;
  professor?: string;
  course?: string;
}

export default class Chart extends React.Component<ChartProps> {
  /*
   * Initialize the grade distribution chart on the webpage.
   */
  theme = {
    tooltip: {
      container: {
        background: 'rgba(0,0,0,.87)',
        color: '#ffffff',
        fontSize: '1.2rem',
        outline: 'none',
        margin: 0,
      }
    }
  }

  /*
   * Create an array of objects to feed into the chart.
   * @return an array of JSON objects detailing the grades for each class
   */
  getClassData = () => {
    let gradeACount = 0, gradeBCount = 0, gradeCCount = 0, gradeDCount = 0,
      gradeFCount = 0, gradePCount = 0, gradeNPCount = 0;

    this.props.gradeData.forEach(data => {
      if ((data.quarter + ' ' + data.year === this.props.quarter || this.props.quarter == 'ALL')
        && (data.instructor === this.props.professor || (data.department + ' ' + data.number) === this.props.course)) {
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
        'id': 'A',
        'label': 'A',
        'A': gradeACount,
        'color': '#2484C6'
      },
      {
        'id': 'B',
        'label': 'B',
        'B': gradeBCount,
        'color': '#54B058'
      },
      {
        'id': 'C',
        'label': 'C',
        'C': gradeCCount,
        'color': '#F9CE50'
      },
      {
        'id': 'D',
        'label': 'D',
        'D': gradeDCount,
        'color': '#ED9237'
      },
      {
        'id': 'F',
        'label': 'F',
        'F': gradeFCount,
        'color': '#E67237'
      },
      {
        'id': 'P',
        'label': 'P',
        'P': gradePCount,
        'color': '#4AB486'
      },
      {
        'id': 'NP',
        'label': 'NP',
        'NP': gradeNPCount,
        'color': '#E36436'
      },
    ];
  }

  /*
   * Indicate how the tooltip should look like when users hover over the bar
   * Code is slightly modified from: https://codesandbox.io/s/nivo-scatterplot-
   * vs-bar-custom-tooltip-7u6qg?file=/src/index.js:1193-1265
   * @param event an event object recording the mouse movement, etc.
   * @return a JSX block styling the chart
   */
  styleTooltip = (event: BarTooltipDatum) => {
    return (
      <div style={this.theme.tooltip.container}>
        <strong>
          {event.data.label}: {eval('event.data.' + event.data.label)}
        </strong>
      </div>
    );
  }

  /*
   * Display the grade distribution chart.
   * @return a JSX block rendering the chart
   */
  render() {
    return <>
      <ResponsiveBar
        data={this.getClassData()}
        keys={['A', 'B', 'C', 'D', 'F', 'P', 'NP']}
        indexBy='label'
        margin={{
          top: 50,
          right: 30,
          bottom: 50,
          left: 30
        }}
        layout='vertical'
        axisBottom={{
          tickSize: 10,
          tickPadding: 5,
          tickRotation: 0,
          legend: 'Grade',
          legendPosition: 'middle',
          legendOffset: 36
        }}
        enableLabel={false}
        colors={getColor}
        theme={this.theme}
        tooltip={this.styleTooltip}
      />
    </>
  }
}
