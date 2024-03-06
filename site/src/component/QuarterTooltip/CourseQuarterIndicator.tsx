import { FC } from 'react';
import { Label, Popup } from 'semantic-ui-react';
import './CourseQuarterIndicator.scss';
import Chart from './Chart';

interface CourseQuarterIndicatorProps {
  terms: string[];
}

const CourseQuarterIndicator: FC<CourseQuarterIndicatorProps> = (props) => {
  const prevYear = new Date().getFullYear() - 1;
  const termsInOrder = props.terms.slice().reverse(); // to show in order of fall, winter, spring, summer

  const summerOfferings = [termsInOrder.includes(`${prevYear} Summer10wk`) && 'Summer Session 10 week', 
    termsInOrder.includes(`${prevYear} Summer1`) && 'Summer Session 1',
    termsInOrder.includes(`${prevYear} Summer2`) && 'Summer Session 2'
  ]

  const offeredLastYear =
  termsInOrder.includes(`${prevYear - 1} Fall`) ||
  termsInOrder.includes(`${prevYear} Winter`) ||
  termsInOrder.includes(`${prevYear} Spring`) ||
  summerOfferings.some((term) => term);

  return (
    <div className="quarter-indicator-container">
      {termsInOrder.length > 0 && (
        <Popup
          trigger={
            <div className="quarter-indicator-row">
              {offeredLastYear ? (
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

                  {(summerOfferings.some((term) => term)) && (
                    <span>
                      {/* <Label circular color="green" empty /> */}
                      <span className="emoji">☀️</span>
                    </span>
                  )}
                </span>
              ) : (
                <Label circular color="grey" empty />
              )}
            </div>
          }
          content={
              <div>
            {offeredLastYear ? (
              <div className="tooltip-column">
                <h5 style={{ marginBottom: '4px' }}>Offered in:</h5>
                {termsInOrder.includes(`${prevYear - 1} Fall`) && (
                  <div >
                    {/* <Label circular color="yellow" empty /> */}
                    <span className="emoji">🍂</span>
                    <span className="emoji-label">Fall {prevYear - 1}</span>
                  </div>
                )}

                {termsInOrder.includes(`${prevYear} Winter`) && (
                  <div >
                    {/* <Label circular color="orange" empty /> */}
                    <span className="emoji">❄️</span>
                    <span className="emoji-label">Winter {prevYear}</span>
                  </div>
                )}

                {termsInOrder.includes(`${prevYear} Spring`) && (
                  <div >
                    {/* <Label circular color="teal" empty /> */}
                    <span className="emoji">🌸</span>
                    <span className="emoji-label">Spring {prevYear}</span>
                  </div>
                )}

                {summerOfferings.some((term) => term) && (
                  <div>
                    {/* <Label circular color="green" empty /> */}
                    <span className="emoji">☀️</span>
                    
                    <span className="emoji-label">{summerOfferings.filter((term) => term).join(', ')} {prevYear}</span>
                  </div>
                )}
              </div>
            ) : (
              <p className="not-offered-text">
                This course was not offered in the {prevYear - 1}-{prevYear} academic year.
              </p>
            )}

            <div className="tooltip-chart-section">
              <h5 style={{ textAlign: 'center' }}>Past offerings</h5>
              <div className="term-chart-container chart">
                <Chart terms={props.terms} />
              </div>
            </div>
            
            </div>
          }
          basic
          position="bottom right"
        />
      )}
    </div>
  );
};

export default CourseQuarterIndicator;
