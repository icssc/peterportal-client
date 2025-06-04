import { FC } from 'react';
import './CourseQuarterIndicator.scss';
import Chart from './Chart';
import { OverlayTrigger, Popover } from 'react-bootstrap';

interface CourseQuarterIndicatorProps {
  terms: string[];
  size: 'xs' | 'sm' | 'lg';
}

const CourseQuarterIndicator: FC<CourseQuarterIndicatorProps> = (props) => {
  // determine the size of all emojis
  const emojiSize = `emoji-${props.size}`;

  // if currently in fall quarter, previous academic year still includes the current
  const prevYear = new Date().getMonth() > 9 ? new Date().getFullYear() : new Date().getFullYear() - 1;

  // show in order of fall, winter, spring, summer
  const termsInOrder = props.terms.slice().reverse();

  // list of all possible summer offerings in the previous year
  const allSummerOfferings = [
    termsInOrder.includes(`${prevYear} Summer10wk`) && 'Summer Session (10 Week)',
    termsInOrder.includes(`${prevYear} Summer1`) && 'Summer Session 1',
    termsInOrder.includes(`${prevYear} Summer2`) && 'Summer Session 2',
  ];

  // filter out summer offerings that weren't offered in the previous year
  const summerOfferings = allSummerOfferings.filter(Boolean);

  // list of all possible offerings in the previous year, with their corresponding emojis and labels
  const allOfferings = [
    { condition: termsInOrder.includes(`${prevYear - 1} Fall`), emoji: '🍂', label: `Fall ${prevYear - 1}` },
    { condition: termsInOrder.includes(`${prevYear} Winter`), emoji: '❄️', label: `Winter ${prevYear}` },
    { condition: termsInOrder.includes(`${prevYear} Spring`), emoji: '🌱', label: `Spring ${prevYear}` },
    { condition: summerOfferings.length > 0, emoji: '☀️', label: `${summerOfferings.join(', ')} ${prevYear}` },
  ];

  // filter out offerings that weren't offered in the previous year
  const prevOfferings = allOfferings.filter((o) => o.condition);

  // check if no offerings were found
  const offeredLastYear = prevOfferings.length > 0;

  // get the total range of years for the past offerings
  const years = termsInOrder.map((term) => parseInt(term.split(' ')[0]));
  const minYear = years.reduce((min, val) => Math.min(min, val), prevYear);
  const maxYear = years.reduce((max, val) => Math.max(max, val), 0);
  const pastOfferingsYear = minYear !== maxYear ? `${minYear} - ${maxYear}` : `${minYear}`;

  const popover = (
    <Popover id="quarter-tooltip-popover">
      <Popover.Content>
        <div className="quarter-tooltip">
          {props.terms.length ? (
            <div>
              {offeredLastYear ? (
                // legend to show terms corresponding to the icons
                <div className="tooltip-column">
                  <h5 style={{ marginBottom: '4px' }}>Last offered in:</h5>
                  {prevOfferings.map((offering, index) => (
                    <div key={index}>
                      <span className={emojiSize}>{offering.emoji}</span>
                      <span className="emoji-label">{offering.label}</span>
                    </div>
                  ))}
                </div>
              ) : (
                // hide legend if course has term data, but not for the previous year
                <p className="not-offered-text">
                  This course was not offered in the {prevYear - 1}-{prevYear} academic year.
                </p>
              )}

              {/* chart of past term offerings */}
              <div className="tooltip-chart-section">
                <h5 style={{ textAlign: 'center' }}>Past Offerings ({pastOfferingsYear})</h5>
                <div className="term-chart-container chart">
                  <Chart terms={props.terms} />
                </div>
              </div>
            </div>
          ) : (
            // hide legend and chart if there is no term data at all
            <p className="not-offered-text">This course has not been offered in any recent years.</p>
          )}
        </div>
      </Popover.Content>
    </Popover>
  );

  return (
    <div className="quarter-indicator-container">
      <OverlayTrigger overlay={popover} placement="auto">
        <div>
          <span className="quarter-indicator-row">
            {offeredLastYear ? (
              prevOfferings.map((quarter, index) => (
                <span key={index} className={emojiSize}>
                  {quarter.emoji}
                </span>
              ))
            ) : (
              <span className={emojiSize}>❌</span>
            )}
          </span>
        </div>
      </OverlayTrigger>
    </div>
  );
};

export default CourseQuarterIndicator;
