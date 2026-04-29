import { Component } from 'react';
import { ResponsivePie, PieTooltipProps } from '@nivo/pie';

import { GradesRaw } from '@peterportal/types';
import ChartTooltip from '../ChartTooltip/ChartTooltip.tsx';
import { getCssVariable } from '../../helpers/styling.ts';

const gradeScale = ['A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-'];
const gpaScale = [4.0, 3.7, 3.3, 3.0, 2.7, 2.3, 2.0, 1.7, 1.3, 1.0, 0, 7];

interface Slice {
  id: 'A' | 'B' | 'C' | 'D' | 'F' | 'P' | 'NP';
  value: number;
  label: string;
  color: string;
}

interface PieProps {
  gradeData: GradesRaw;
  quarter: string;
  professor?: string;
  course?: string;
}

interface GradeAggregateData {
  gradeACount: number;
  gradeBCount: number;
  gradeCCount: number;
  gradeDCount: number;
  gradeFCount: number;
  gradePCount: number;
  gradeNPCount: number;
  total: number;
  totalPNP: number;
  averageGPA: string;
  averageGrade: string;
  averagePNP: string;
}

function gpaToGradeConverter(gpa: string): string {
  let i;
  for (i = 0; Number(gpa) < gpaScale[i]; i++);
  return gradeScale[i];
}

export function getAggregateClassGradeData(
  gradeData: GradesRaw,
  professor: string | undefined,
  quarter: string,
  course: string | undefined,
): GradeAggregateData {
  const classGradeData: GradeAggregateData = {
    gradeACount: 0,
    gradeBCount: 0,
    gradeCCount: 0,
    gradeDCount: 0,
    gradeFCount: 0,
    gradePCount: 0,
    gradeNPCount: 0,
    total: 0,
    totalPNP: 0,
    averageGPA: '',
    averageGrade: '',
    averagePNP: '',
  };

  let sum = 0;

  gradeData.forEach((data) => {
    const quarterMatch = quarter === 'ALL' || data.quarter + ' ' + data.year === quarter;
    const profMatch = professor === 'ALL' || data.instructors.includes(professor ?? '');
    const courseMatch = course === 'ALL' || data.department + ' ' + data.courseNumber === course;
    if (quarterMatch && (profMatch || courseMatch)) {
      classGradeData.gradeACount += data.gradeACount;
      classGradeData.gradeBCount += data.gradeBCount;
      classGradeData.gradeCCount += data.gradeCCount;
      classGradeData.gradeDCount += data.gradeDCount;
      classGradeData.gradeFCount += data.gradeFCount;
      classGradeData.gradePCount += data.gradePCount;
      classGradeData.gradeNPCount += data.gradeNPCount;
      sum += 4.0 * data.gradeACount + 3.0 * data.gradeBCount + 2.0 * data.gradeCCount + 1.0 * data.gradeDCount;
      classGradeData.total +=
        data.gradeACount +
        data.gradeBCount +
        data.gradeCCount +
        data.gradeDCount +
        data.gradeFCount +
        data.gradePCount +
        data.gradeNPCount;
      classGradeData.totalPNP += data.gradePCount + data.gradeNPCount;

      if (data.gradePCount >= data.gradeNPCount) {
        classGradeData.averagePNP = 'P';
      } else {
        classGradeData.averagePNP = 'NP';
      }
    }
  });

  classGradeData.averageGPA = (sum / (classGradeData.total - classGradeData.totalPNP)).toFixed(1);
  classGradeData.averageGrade = gpaToGradeConverter(classGradeData.averageGPA);

  return classGradeData;
}

export default class Pie extends Component<PieProps> {
  total = 0;
  totalPNP = 0;
  averageGPA = '';
  averageGrade = '';
  averagePNP = '';

  getClassData = (): Slice[] => {
    const { professor, quarter, course } = this.props;

    const aggregateClassGradeData = getAggregateClassGradeData(this.props.gradeData, professor, quarter, course);
    this.total = aggregateClassGradeData.total;
    this.totalPNP = aggregateClassGradeData.totalPNP;
    this.averageGPA = aggregateClassGradeData.averageGPA;
    this.averageGrade = aggregateClassGradeData.averageGrade;
    this.averagePNP = aggregateClassGradeData.averagePNP;

    const pnpData: Slice[] = [
      {
        id: 'P',
        label: 'P',
        value: aggregateClassGradeData.gradePCount,
        color: getCssVariable('--mui-palette-chart-pass'),
      },
      {
        id: 'NP',
        label: 'NP',
        value: aggregateClassGradeData.gradeNPCount,
        color: getCssVariable('--mui-palette-chart-noPass'),
      },
    ];

    if (this.totalPNP == this.total) {
      return pnpData;
    }

    const gradeData: Slice[] = [
      {
        id: 'A',
        label: 'A',
        value: aggregateClassGradeData.gradeACount,
        color: getCssVariable('--mui-palette-chart-blue'),
      },
      {
        id: 'B',
        label: 'B',
        value: aggregateClassGradeData.gradeBCount,
        color: getCssVariable('--mui-palette-chart-green'),
      },
      {
        id: 'C',
        label: 'C',
        value: aggregateClassGradeData.gradeCCount,
        color: getCssVariable('--mui-palette-chart-yellow'),
      },
      {
        id: 'D',
        label: 'D',
        value: aggregateClassGradeData.gradeDCount,
        color: getCssVariable('--mui-palette-chart-orange'),
      },
      {
        id: 'F',
        label: 'F',
        value: aggregateClassGradeData.gradeFCount,
        color: getCssVariable('--mui-palette-chart-red'),
      },
    ];

    return gradeData.concat(pnpData).filter((slice) => slice.value !== 0);
  };

  styleTooltip = (props: PieTooltipProps<Slice>) => {
    const gradePercent = ((props.datum.value / this.total) * 100).toFixed(2) + '%';
    return <ChartTooltip label={props.datum.id} value={gradePercent} />;
  };

  render() {
    const gradeDistribution = this.getClassData();
    return (
      <div style={{ width: '100%', position: 'relative' }}>
        <ResponsivePie<Slice>
          data={gradeDistribution}
          margin={{
            top: 50,
            bottom: 50,
            left: 15,
            right: 15,
          }}
          enableArcLabels={false}
          enableArcLinkLabels={false}
          innerRadius={0.8}
          padAngle={2}
          colors={gradeDistribution.map((grade) => grade.color)}
          cornerRadius={3}
          borderWidth={1}
          borderColor={{ from: 'color', modifiers: [['darker', 0.2]] }}
          tooltip={this.styleTooltip}
        />
        <div
          style={{
            position: 'absolute',
            pointerEvents: 'none',
            top: '50%',
            left: '50%',
            transform: 'translate(-50%, -50%)',
            textAlign: 'center',
            width: '100%',
          }}
        >
          {this.totalPNP == this.total ? <h3 className="pie-text">Average Grade: {this.averagePNP}</h3> : null}
          {this.totalPNP != this.total ? (
            <h3 className="pie-text">
              Average Grade: {this.averageGrade} ({this.averageGPA})
            </h3>
          ) : null}
          <h3 className="pie-text" style={{ marginBottom: '6px' }}>
            Total Enrolled: <strong>{this.total}</strong>
          </h3>
          {this.totalPNP > 0 ? <small>{this.totalPNP} enrolled as P/NP</small> : null}
        </div>
      </div>
    );
  }
}
