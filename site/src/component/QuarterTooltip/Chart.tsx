import React from 'react';
import { ResponsiveBar, BarTooltipProps, BarDatum } from '@nivo/bar';

import ThemeContext from '../../style/theme-context';
import ChartTooltip from '../ChartTooltip/ChartTooltip.tsx';
import { getTheme, getCssVariable } from '../../helpers/styling.ts';

interface ChartProps {
  terms: string[];
}

export default class Chart extends React.Component<ChartProps> {
  /*
   * Create an array of objects to feed into the chart.
   * @return an array of JSON objects detailing the grades for each class
   */
  getTermData = (): BarDatum[] => {
    const termCounts = {
      Fall: 0,
      Winter: 0,
      Spring: 0,
    };

    // for summer, count unique years rather than total terms (e.g. count SS1 2023 and SS2 2023 as one)
    const summerYears = new Set<string>();

    this.props.terms.forEach((data) => {
      const [year, term] = data.split(' ');
      if (term === 'Fall' || term === 'Winter' || term === 'Spring') {
        termCounts[term]++;
      } else if (term.startsWith('Summer')) {
        summerYears.add(year);
      }
    });

    return [
      {
        id: 'fall',
        label: 'Fall',
        fall: termCounts.Fall,
        color: getCssVariable('--red-secondary-light'),
      },
      {
        id: 'winter',
        label: 'Winter',
        winter: termCounts.Winter,
        color: getCssVariable('--blue-secondary-light'),
      },
      {
        id: 'spring',
        label: 'Spring',
        spring: termCounts.Spring,
        color: getCssVariable('--green-secondary-light'),
      },
      {
        id: 'summer',
        label: 'Summer',
        summer: summerYears.size,
        color: getCssVariable('--yellow-secondary-light'),
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
    return <ChartTooltip label={props.label} value={props.data[props.label]} />;
  };

  /*
   * Display the grade distribution chart.
   * @return a JSX block rendering the chart
   */
  render() {
    const data = this.getTermData();

    // greatestCount calculates the upper bound of the graph (i.e. the greatest number of students in a single grade)
    const greatestCount = data.reduce(
      (max, term) => ((term[term.id] as number) > max ? (term[term.id] as number) : max),
      0,
    );

    // The base marginX is 30, with increments of 5 added on for every order of magnitude greater than 100 to accomadate for larger axis labels (1,000, 10,000, etc)
    // For example, if greatestCount is 5173 it is (when rounding down (i.e. floor)), one magnitude (calculated with log_10) greater than 100, therefore we add one increment of 5px to our base marginX of 30px
    // Math.max() ensures that we're not finding the log of a non-positive number
    const marginX = 30 + 5 * Math.floor(Math.log10(Math.max(100, greatestCount) / 100));

    //tickSize calculates the proper scale of the graph based on its upper bound as calculated in GreatestCount
    const tickSize = Math.floor(greatestCount / 6) > 1 ? Math.floor(greatestCount / 6) : 1;

    return (
      <>
        <ThemeContext.Consumer>
          {({ darkMode }) => (
            <ResponsiveBar
              data={data}
              keys={data.map((datum) => String(datum.id))}
              colors={data.map((datum) => String(datum.color))}
              margin={{
                top: 25,
                right: marginX,
                bottom: 25,
                left: marginX,
              }}
              layout="vertical"
              axisBottom={{
                tickSize: 5,
                tickPadding: 5,
                tickRotation: 0,
                legend: 'Term',
                legendPosition: 'middle',
                legendOffset: 36,
              }}
              axisLeft={{
                tickValues: Array.from({ length: greatestCount / tickSize }, (_, i) => i * tickSize + tickSize),
              }}
              enableLabel={false}
              theme={getTheme(darkMode)}
              tooltipLabel={(datum) => String(datum.id)}
              tooltip={this.styleTooltip}
            />
          )}
        </ThemeContext.Consumer>
      </>
    );
  }
}
