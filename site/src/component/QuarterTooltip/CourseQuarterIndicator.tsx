import { FC } from 'react';
import { Label, Popup } from 'semantic-ui-react';
import './CourseQuarterIndicator.scss';
import Chart from './Chart';

interface CourseQuarterIndicatorProps {
  terms: string[];
}

const CourseQuarterIndicator: FC<CourseQuarterIndicatorProps> = (props) => {
  const prevYear = new Date().getFullYear() - 1;

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

  return (
    <div className="quarter-indicator-container">
      <Popup
        trigger={
          <div className="quarter-indicator-row">
            {offeredLastYear ? (
              // icons to show which terms were offered last year
              <span>
                {termsInOrder.includes(`${prevYear - 1} Fall`) && (
                  <span>
                    {/* <Label circular color="yellow" empty /> */}
                    <span className="emoji">🍂</span>
                  </span>
                )}

                {termsInOrder.includes(`${prevYear} Winter`) && (
                  <span>
                    {/* <Label circular color="orange" empty /> */}
                    <span className="emoji">❄️</span>
                  </span>
                )}

                {termsInOrder.includes(`${prevYear} Spring`) && (
                  <span>
                    {/* <Label circular color="teal" empty /> */}
                    <span className="emoji">🌸</span>
                  </span>
                )}

                {
                  // summer icon shows if there was any summer session offering
                  summerOfferings.some((term) => term) && (
                    <span>
                      {/* <Label circular color="green" empty /> */}
                      <span className="emoji">☀️</span>
                    </span>
                  )
                }
              </span>
            ) : (
              // no offerings from last year, no icons to show
              <Label circular color="grey" empty />
            )}
          </div>
        }
        content={
          <div>
            {props.terms.length ? (
              <div>
                {offeredLastYear ? (
                  // legend to show terms corresponding to the icons
                  <div className="tooltip-column">
                    <h5 style={{ marginBottom: '4px' }}>Last offered in:</h5>
                    {termsInOrder.includes(`${prevYear - 1} Fall`) && (
                      <div>
                        {/* <Label circular color="yellow" empty /> */}
                        <span className="emoji">🍂</span>
                        <span className="emoji-label">Fall {prevYear - 1}</span>
                      </div>
                    )}

                    {termsInOrder.includes(`${prevYear} Winter`) && (
                      <div>
                        {/* <Label circular color="orange" empty /> */}
                        <span className="emoji">❄️</span>
                        <span className="emoji-label">Winter {prevYear}</span>
                      </div>
                    )}

                    {termsInOrder.includes(`${prevYear} Spring`) && (
                      <div>
                        {/* <Label circular color="teal" empty /> */}
                        <span className="emoji">🌸</span>
                        <span className="emoji-label">Spring {prevYear}</span>
                      </div>
                    )}

                    {summerOfferings.some((term) => term) && (
                      <div>
                        {/* <Label circular color="green" empty /> */}
                        <span className="emoji">☀️</span>

                        <span className="emoji-label">
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
                  <h5 style={{ textAlign: 'center' }}>All Past Offerings</h5>
                  <div className="term-chart-container chart">
                    <Chart terms={props.terms} />
                  </div>
                </div>
              </div>
            ) : (
              // hide legend and chart if there is no term data at all
              // <p className="not-offered-text">There is no data on this course's past offerings.</p>
              <p className="not-offered-text">This course has not been offered in any recent years.</p>
            )}
          </div>
        }
        basic
        position="bottom right"
      />
    </div>
  );
};

export default CourseQuarterIndicator;
