import { FC } from 'react';
import { Check } from 'react-bootstrap-icons';
import './RecentOfferings.scss';

interface RecentOfferingsProps {
  terms: string[];
}

function parseOfferings(terms: string[]): { [academicYear: string]: boolean[] } {
  const offerings: { [academicYear: string]: boolean[] } = {};

  for (const term of terms) {
    const [yearStr, quarter] = term.split(' ');
    const year = parseInt(yearStr, 10);

    let quarterIndex = -1;
    if (quarter.startsWith('Fall')) quarterIndex = 0;
    else if (quarter.startsWith('Winter')) quarterIndex = 1;
    else if (quarter.startsWith('Spring')) quarterIndex = 2;
    else if (quarter.startsWith('Summer')) quarterIndex = 3;
    else continue;

    // if the course is not described as a "Fall" course, it should be listed as the previous academic year
    // e.g. "Winter 2023" should be in "2022-2023", but "Fall 2023" should be in "2023-2024"
    const academicYear = quarterIndex === 0 ? `${year}-${year + 1}` : `${year - 1}-${year}`;

    if (!offerings[academicYear]) {
      offerings[academicYear] = [false, false, false, false]; // Fall, Winter, Spring, Summer
    }

    offerings[academicYear][quarterIndex] = true;
  }

  return offerings;
}

const RecentOfferings: FC<RecentOfferingsProps> = (props) => {
  //show in order of fall, winter, spring, summer
  const termsInOrder = props.terms.slice().reverse();

  const offerings = parseOfferings(termsInOrder);

  return (
    <div className="recent-offerings-container">
      <h2>Recent Offerings</h2>

      <table className="recent-offerings-table">
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
            .map(([year, quarters]) => (
              <tr key={year}>
                <td>{year}</td>
                {quarters.map((offered, index) => (
                  <td key={index}>{offered ? <Check /> : null}</td>
                ))}
              </tr>
            ))}
        </tbody>
      </table>
    </div>
  );
};

export default RecentOfferings;
