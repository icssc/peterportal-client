import { useContext } from 'react';
import { BarChart, Bar, XAxis, Tooltip, ResponsiveContainer, LabelList } from 'recharts';
import ThemeContext from '../../style/theme-context';
import { GradesRaw } from '@peterportal/types';
import { getAggregateGradeData } from '../../helpers/gradeDist.ts';
import ChartTooltip from '../ChartTooltip/ChartTooltip.tsx';
import { getCssVariable } from '../../helpers/styling.ts';
import './Chart.scss';

interface ChartProps {
  gradeData: GradesRaw;
  quarter: string;
  professor?: string;
  course?: string;
}

const GRADE_COLORS: Record<string, string> = {
  A: '--mui-palette-chart-blue',
  B: '--mui-palette-chart-green',
  C: '--mui-palette-chart-yellow',
  D: '--mui-palette-chart-orange',
  F: '--mui-palette-chart-red',
  P: '--mui-palette-chart-pass',
  NP: '--mui-palette-chart-noPass',
};

export default function Chart({ gradeData, quarter, professor, course }: ChartProps) {
  const { darkMode } = useContext(ThemeContext);

  const aggregateGradeData = getAggregateGradeData(gradeData, professor, quarter, course);

  const data = [
    { grade: 'A', count: aggregateGradeData.gradeACount, fill: getCssVariable(GRADE_COLORS.A) },
    { grade: 'B', count: aggregateGradeData.gradeBCount, fill: getCssVariable(GRADE_COLORS.B) },
    { grade: 'C', count: aggregateGradeData.gradeCCount, fill: getCssVariable(GRADE_COLORS.C) },
    { grade: 'D', count: aggregateGradeData.gradeDCount, fill: getCssVariable(GRADE_COLORS.D) },
    { grade: 'F', count: aggregateGradeData.gradeFCount, fill: getCssVariable(GRADE_COLORS.F) },
    { grade: 'P', count: aggregateGradeData.gradePCount, fill: getCssVariable(GRADE_COLORS.P) },
    { grade: 'NP', count: aggregateGradeData.gradeNPCount, fill: getCssVariable(GRADE_COLORS.NP) },
  ];

  return (
    <ResponsiveContainer width="100%" height="100%">
      <BarChart data={data} margin={{ top: 50, right: 30, bottom: 50, left: 30 }}>
        <XAxis dataKey="grade" />
        <Tooltip
          cursor={{ fill: darkMode ? 'rgba(255,255,255,0.05)' : 'rgba(0,0,0,0.05)' }}
          content={({ active, payload }) => {
            if (!active || !payload?.length) return null;
            const { grade, count } = payload[0].payload;
            return <ChartTooltip label={grade} value={count} />;
          }}
        />
        <Bar dataKey="count" radius={[8, 8, 0, 0]}>
          <LabelList dataKey="count" position="top" fill={getCssVariable('--mui-palette-text-secondary')} />
        </Bar>
      </BarChart>
    </ResponsiveContainer>
  );
}
