import { FC } from 'react';
import './CourseQuarterIndicator.scss';
import Chart from './Chart';
import { OverlayTrigger, Popover } from 'react-bootstrap';

interface CourseQuarterIndicatorProps {
  terms: string[];
  size: 'xs' | 'sm' | 'lg';
}

const CourseQuarterIndicator: FC<CourseQuarterIndicatorProps> = (props) => {
  const emojiSize = `emoji-${props.size}`;

  // if currently in fall quarter, previous academic year still includes the current
  const prevYear = new Date().getMonth() > 9 ? new Date().getFullYear() : new Date().getFullYear() - 1;

  // show in order of fall, winter, spring, summer
  const termsInOrder = props.terms.slice().reverse();

  const summerOfferings = [
    termsInOrder.includes(`${prevYear} Summer10wk`) && 'Summer Session (10 Week)',
    termsInOrder.includes(`${prevYear} Summer1`) && 'Summer Session 1',
    termsInOrder.includes(`${prevYear} Summer2`) && 'Summer Session 2',
  ];

  // check if the course was offered in the previous academic year at any term
  const offeredLastYear =
    termsInOrder.includes(`${prevYear - 1} Fall`) ||
    termsInOrder.includes(`${prevYear} Winter`) ||
    termsInOrder.includes(`${prevYear} Spring`) ||
    // if the course was offered in any summer session from last year
    summerOfferings.some((term) => term);

  // find min and max year in term range
  const years = termsInOrder.map((term) => parseInt(term.split(' ')[0]));
  const minYear = years.reduce((min, val) => Math.min(min, val), prevYear);
  const maxYear = years.reduce((max, val) => Math.max(max, val), 0);

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
                  {termsInOrder.includes(`${prevYear - 1} Fall`) && (
                    <div>
                      <span className={emojiSize}>🍂</span>
                      <span className="emoji-label">Fall {prevYear - 1}</span>
                    </div>
                  )}

                  {termsInOrder.includes(`${prevYear} Winter`) && (
                    <div>
                      <span className={emojiSize}>❄️</span>
                      <span className="emoji-label">Winter {prevYear}</span>
                    </div>
                  )}

                  {termsInOrder.includes(`${prevYear} Spring`) && (
                    <div>
                      <span className={emojiSize}>🌱</span>
                      <span className="emoji-label">Spring {prevYear}</span>
                    </div>
                  )}

                  {summerOfferings.some((term) => term) && (
                    <div>
                      <span className={emojiSize}>☀️</span>
                      <span className="emoji-label">
                        {/* list out summer sessions offered */}
                        {summerOfferings.filter((term) => term).join(', ')} {prevYear}
                      </span>
                    </div>
                  )}
                </div>
              ) : (
                // hide legend if course has term data, but not for the previous year
                <p className="not-offered-text">
                  This course was not offered in the {prevYear - 1}-{prevYear} academic year.
                </p>
              )}

              {/* chart of past term offerings */}
              <div className="tooltip-chart-section">
                <h5 style={{ textAlign: 'center' }}>
                  Past Offerings ({minYear !== maxYear ? `${minYear} - ${maxYear}` : `${minYear}`})
                </h5>
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
          {offeredLastYear ? (
            // icons to show which terms were offered last year
            <span className="quarter-indicator-row">
              {termsInOrder.includes(`${prevYear - 1} Fall`) && (
                <span>
                  <span className={emojiSize}>🍂</span>
                </span>
              )}

              {termsInOrder.includes(`${prevYear} Winter`) && (
                <span>
                  <span className={emojiSize}>❄️</span>
                </span>
              )}

              {termsInOrder.includes(`${prevYear} Spring`) && (
                <span>
                  <span className={emojiSize}>🌱</span>
                </span>
              )}

              {
                // summer icon shows if there was any summer session offering
                summerOfferings.some((term) => term) && (
                  <span>
                    <span className={emojiSize}>☀️</span>
                  </span>
                )
              }
            </span>
          ) : (
            // no offerings from last year, no icons to show
            <div>
              <span className="quarter-indicator-row">
                <span>
                  <span className={emojiSize}>❌</span>
                </span>
              </span>
            </div>
          )}
        </div>
      </OverlayTrigger>
    </div>
  );
};

export default CourseQuarterIndicator;
