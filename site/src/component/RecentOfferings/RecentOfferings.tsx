import { FC, useState, useEffect } from 'react';
import './RecentOfferings.scss';
import { isTermAfter } from '../../helpers/util';
import trpc from '../../trpc';
import { useTheme } from '@mui/material/styles';
import CheckIcon from '@mui/icons-material/Check';
import QuestionMarkIcon from '@mui/icons-material/QuestionMark';

interface RecentOfferingsProps {
  terms: string[];
}

interface Offerings {
  [academicYear: string]: boolean[];
}

function parseOfferings(terms: string[]): Offerings {
  const offerings: Offerings = {};

  for (const term of terms) {
    const [yearStr, quarter] = term.split(' ');
    const year = parseInt(yearStr, 10);

    let quarterIndex = -1;
    if (quarter.startsWith('Fall')) quarterIndex = 0;
    else if (quarter.startsWith('Winter')) quarterIndex = 1;
    else if (quarter.startsWith('Spring')) quarterIndex = 2;
    else if (quarter.startsWith('Summer')) quarterIndex = 3;
    else continue;

    // If the course is not described as a "Fall" course, it should be listed as starting in the previous academic year
    // e.g. "Winter 2023" should be in "2022-2023", but "Fall 2023" should be in "2023-2024"
    const startYear = quarterIndex === 0 ? `${year}-${year + 1}` : `${year - 1}-${year}`;

    // Initialize Fall, Winter, Spring, Summer of the year to false
    offerings[startYear] ??= [false, false, false, false];

    offerings[startYear][quarterIndex] = true;
  }

  // Sort offerings by academic year in descending order
  const sortedOfferings: Offerings = Object.fromEntries(
    Object.entries(offerings).sort(([yearA], [yearB]) => yearB.localeCompare(yearA)),
  );

  return sortedOfferings;
}

const RecentOfferings: FC<RecentOfferingsProps> = (props) => {
  // Show in order of Fall, Winter, Spring, Summer
  const termsInOrder = props.terms.slice().reverse();
  const offerings = parseOfferings(termsInOrder);
  const [currentTerm, setCurrentTerm] = useState<string>('');
  const quarterLabels = ['Fall', 'Winter', 'Spring', 'Summer'];
  const theme = useTheme();

  useEffect(() => {
    trpc.schedule.currentQuarter.query().then((data) => {
      setCurrentTerm(data);
    });
  }, []);

  return (
    <div className="recent-offerings">
      <h2>Recent Offerings</h2>

      <table className="ppc-table recent-offerings-table">
        <thead>
          <tr>
            <th>Academic Year</th>
            <th>🍂</th>
            <th>❄️</th>
            <th>🌸</th>
            <th>☀️</th>
          </tr>
        </thead>
        <tbody>
          {/* only display the last 4 years of offerings */}
          {Object.entries(offerings)
            .slice(0, 4)
            .map(([year, quarters]) => {
              const [startYear, endYear] = year.split('-');
              return (
                <tr key={year}>
                  <td>{year}</td>
                  {quarters.map((offered, index) => {
                    const quarterName = quarterLabels[index];
                    const term = `${index === 0 ? startYear : endYear} ${quarterName}`; // Parse an entry to a term string to see if it's in the future
                    const isFutureTerm = currentTerm && isTermAfter(term, currentTerm);

                    return (
                      <td key={index}>
                        {offered ? (
                          <CheckIcon style={{ fontSize: 20 }} />
                        ) : isFutureTerm ? (
                          <QuestionMarkIcon style={{ fontSize: 20, color: theme.palette.text.secondary }} />
                        ) : null}
                      </td>
                    );
                  })}
                </tr>
              );
            })}
        </tbody>
      </table>
    </div>
  );
};

export default RecentOfferings;
