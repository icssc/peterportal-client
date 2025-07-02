import { FC } from 'react';
import './RecentOfferingsTable.scss';
import CheckIcon from '@mui/icons-material/Check';
import { QuarterName } from '@peterportal/types';

interface Offerings {
  [academicYear: string]: boolean[];
}

function parseOfferings(terms: string[]) {
  const offerings: Offerings = {};

  for (const term of terms) {
    const [yearStr, quarter] = term.split(' ') as [string, QuarterName];
    const year = parseInt(yearStr);

    const quarterIndexMap = {
      Fall: 0,
      Winter: 1,
      Spring: 2,
      Summer10wk: 3,
      Summer1: 3,
      Summer2: 3,
    };
    const quarterIndex = quarterIndexMap[quarter];

    // If the course is not described as a "Fall" course, it should be listed as starting in the previous academic year
    // e.g. "Winter 2023" should be in "2022-2023", but "Fall 2023" should be in "2023-2024"
    const startYear = quarterIndex === 0 ? `${year}-${year + 1}` : `${year - 1}-${year}`;

    // Initialize each quarter (Fall, Winter, Spring, Summer) as false
    offerings[startYear] ??= [false, false, false, false];

    offerings[startYear][quarterIndex] = true;
  }

  // Sort offerings by academic year in descending order
  const sortedOfferings = Object.entries(offerings).sort(([yearA], [yearB]) => yearB.localeCompare(yearA));
  const top4Offerings = sortedOfferings.slice(0, 4);

  return top4Offerings;
}

interface RecentOfferingsTableProps {
  terms: string[];
  size: 'thin' | 'wide';
}

const RecentOfferingsTable: FC<RecentOfferingsTableProps> = ({ terms, size }) => {
  if (terms.length === 0) return null;

  const offerings = parseOfferings(terms);

  const getYearColumnValue = (year: string) => {
    if (size === 'wide') return year;
    const [start, end] = year.split('-');
    return `${start.substring(2)}-${end.substring(2)}`;
  };

  return (
    <div className="recent-offerings">
      <h2 className={size === 'thin' ? 'center' : ''}>Recent Offerings</h2>

      <table className="ppc-table recent-offerings-table">
        <thead>
          <tr>
            <th>{size === 'thin' ? 'Year' : 'Academic Year'}</th>
            <th>🍂</th>
            <th>❄️</th>
            <th>🌸</th>
            <th>☀️</th>
          </tr>
        </thead>
        <tbody>
          {offerings.map(([year, quarters]) => (
            <tr key={year}>
              <td>{getYearColumnValue(year)}</td>
              {quarters.map((offered, index) => (
                <td key={index}>{offered && <CheckIcon style={{ fontSize: 20 }} />}</td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default RecentOfferingsTable;
