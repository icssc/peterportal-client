import React from 'react';
import { ResponsivePie, PieTooltipProps } from '@nivo/pie';

import { GradeDistData } from '../../types/types';

const gradeScale = ['A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-']
const gpaScale = [4.0, 3.7, 3.3, 3.0, 2.7, 2.3, 2.0, 1.7, 1.3, 1.0, 0, 7]

interface Slice {
  id: 'A' | 'B' | 'C' | 'D' | 'F';
  value: number;
  label: string;
  color: string;
}

interface PieProps {
  gradeData: GradeDistData;
  quarter: string;
  professor?: string;
  course?: string;
}

export default class Pie extends React.Component<PieProps> {
  total = 0;
  totalPNP = 0;
  averageGPA = '';
  averageGrade = '';

  getClassData = () => {
    let gradeACount = 0, gradeBCount = 0, gradeCCount = 0, gradeDCount = 0,
      gradeFCount = 0, gradePCount = 0, gradeNPCount = 0;

    this.total = 0;
    this.totalPNP = 0;
    this.averageGPA = '';
    this.averageGrade = '';

    var sum = 0

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
        sum += ((4.0 * data.gradeACount) + (3.0 * data.gradeBCount) + (2.0 * data.gradeCCount) + (1.0 * data.gradeDCount))
        this.total += data.gradeACount + data.gradeBCount + data.gradeCCount
          + data.gradeDCount + data.gradeFCount + data.gradePCount + data.gradeNPCount;
        this.totalPNP += data.gradePCount + data.gradeNPCount;
      }
    });

    this.averageGPA = (sum / (this.total - this.totalPNP)).toFixed(1)
    this.gpaToGradeConverter(this.averageGPA);

    let data: Slice[] = [
      {
        'id': 'A',
        'label': 'A',
        'value': gradeACount,
        'color': '#60A3D1'
      },
      {
        'id': 'B',
        'label': 'B',
        'value': gradeBCount,
        'color': '#81C284'
      },
      {
        'id': 'C',
        'label': 'C',
        'value': gradeCCount,
        'color': '#F5D77F'
      },
      {
        'id': 'D',
        'label': 'D',
        'value': gradeDCount,
        'color': '#ECAD6D'
      },
      {
        'id': 'F',
        'label': 'F',
        'value': gradeFCount,
        'color': '#E8966D'
      }
      //   {
      //     'id': 'P',
      //     'label': 'P',
      //     'value': gradePCount,
      //     'color': '#4AB486'
      //   },
      //   {
      //     'id': 'NP',
      //     'label': 'NP',
      //     'value': gradeNPCount,
      //     'color': '#E36436'
      //   },
    ];
    return data;
  }

  gpaToGradeConverter(gpa: string) {
    for (var i = 0; Number(gpa) < gpaScale[i]; i++) { }
    this.averageGrade = gradeScale[i];
  }

  render() {
    return (
      <div style={{ width: '100%' }}>
        <ResponsivePie<Slice>
          data={this.getClassData()}
          margin={{
            top: 50,
            bottom: 50,
            left: 15,
            right: 15
          }}
          enableArcLabels={false}
          enableArcLinkLabels={false}
          innerRadius={0.8}
          padAngle={2}
          colors={['#60A3D1', '#81C284', '#F5D77F', '#ECAD6D', '#E8966D']}
          cornerRadius={3}
          borderWidth={1}
          borderColor={{ from: 'color', modifiers: [['darker', 0.2]] }}
          tooltip={(props: PieTooltipProps<Slice>) => (
            <div style={{ 
              color: '#FFFFFF',
              background: '#000000',
              paddingLeft: '0.5em',
              paddingRight: '0.5em'
            }}>
              <strong>
                {props.datum.id}: {(props.datum.value / (this.total - this.totalPNP) * 100).toFixed(2)}%
              </strong>
            </div>
          )}
        />
        <div style={{ display: 'flex', textAlign: 'center', margin: '-235px' }}>
          <div style={{ margin: 'auto' }}>
            <h3 className='pie-text'>Average Grade: {this.averageGrade} ({this.averageGPA})</h3>
            <h3 className='pie-text' style={{ marginBottom: '6px' }}>Total Enrolled: <strong>{this.total}</strong></h3>
            {this.totalPNP > 0 ? <small>{this.totalPNP} enrolled as P/NP</small> : null}
          </div>
        </div>
      </div>
    )
  }
}

