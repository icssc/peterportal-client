import { Component } from 'react';
import { ResponsivePie, PieTooltipProps } from '@nivo/pie';

import { GradesRaw, LetterGrade, letterGrades, pnpGrades } from '@peterportal/types';
import { DataType } from '../../types/types';
import ChartTooltip from '../ChartTooltip/ChartTooltip.tsx';
import { getCssVariable } from '../../helpers/styling.ts';

const gradeScale = ['A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-'];
const gpaScale = [4.0, 3.7, 3.3, 3.0, 2.7, 2.3, 2.0, 1.7, 1.3, 1.0, 0, 7];

interface Slice {
  id: LetterGrade;
  value: number;
  label: string;
  color: string;
}

interface PieProps {
  gradeData: GradesRaw;
  quarter: string;
  data: string;
  dataType: DataType;
}

export default class Pie extends Component<PieProps> {
  total = 0;
  totalPNP = 0;
  averageGPA = '';
  averageGrade = '';
  averagePNP = '';

  getClassData = (): Slice[] => {
    this.total = 0;
    this.totalPNP = 0;
    this.averageGPA = '';
    this.averageGrade = '';
    this.averagePNP = '';
    let sum = 0;

    const { gradeData, dataType, data, quarter } = this.props;

    const gradeCounts = {
      A: 0,
      B: 0,
      C: 0,
      D: 0,
      F: 0,
      P: 0,
      NP: 0,
    };
    const gradeColorVars = {
      A: '--blue-secondary-light',
      B: '--green-secondary-light',
      C: '--yellow-secondary-light',
      D: '--orange-secondary-light',
      F: '--red-secondary-light',
      P: '--gradedist-p',
      NP: '--gradedist-np',
    };

    gradeData.forEach((entry) => {
      const correctQuarter = quarter === 'ALL' || `${entry.quarter} ${entry.year}` === quarter;
      const correctData =
        dataType === 'professor'
          ? entry.instructors.includes(data)
          : `${entry.department} ${entry.courseNumber}` === data;

      if (correctQuarter && correctData) {
        const getGradeCount = (grade: LetterGrade) => entry[`grade${grade}Count` as keyof typeof entry] as number;
        letterGrades.forEach((grade) => {
          gradeCounts[grade] += getGradeCount(grade);
        });
        this.total += letterGrades.reduce((acc, grade) => acc + getGradeCount(grade), 0);
        this.totalPNP += entry.gradePCount + entry.gradeNPCount;
        this.averagePNP = entry.gradePCount >= entry.gradeNPCount ? 'P' : 'NP';
        sum += 4.0 * entry.gradeACount + 3.0 * entry.gradeBCount + 2.0 * entry.gradeCCount + 1.0 * entry.gradeDCount;
      }
    });

    this.averageGPA = (sum / (this.total - this.totalPNP)).toFixed(1);
    const gpaIndex = gpaScale.findIndex((scale) => Number(this.averageGPA) < scale);
    this.averageGrade = gradeScale[gpaIndex];

    const createSlice = (grade: LetterGrade): Slice => ({
      id: grade,
      label: grade,
      value: gradeCounts[grade],
      color: getCssVariable(gradeColorVars[grade]),
    });

    const dataset = this.totalPNP === this.total ? pnpGrades : letterGrades;
    return dataset.map(createSlice).filter((slice) => slice.value !== 0);
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
          <h3 className="pie-text">
            Average Grade:{' '}
            {this.totalPNP === this.total ? this.averagePNP : `${this.averageGrade} (${this.averageGPA})`}
          </h3>
          <h3 className="pie-text" style={{ marginBottom: '6px' }}>
            Total Enrolled: <strong>{this.total}</strong>
          </h3>
          {this.totalPNP > 0 && <small>{this.totalPNP} enrolled as P/NP</small>}
        </div>
      </div>
    );
  }
}
