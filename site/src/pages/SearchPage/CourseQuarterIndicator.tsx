import { FC } from 'react';
import { Label, Popup } from 'semantic-ui-react';

interface CourseQuarterIndicatorProps {
  terms: string[];
}

const CourseQuarterIndicator: FC<CourseQuarterIndicatorProps> = (props) => {
  return (
    <div style={{ display: 'flex', marginLeft: 'auto' }}>
      {props.terms.length > 0 && (
        <Popup
          trigger={
            <div style={{ display: 'inline' }}>
              {props.terms.includes('2020 Fall') && (
                <span style={{ float: 'right', marginLeft: '4px' }}>
                  <Label circular color="yellow" empty />
                </span>
              )}

              {props.terms.includes('2020 Summer2') && (
                <span style={{ float: 'right', marginLeft: '4px' }}>
                  <Label circular color="violet" empty />
                </span>
              )}

              {props.terms.includes('2020 Summer10wk') && (
                <span style={{ float: 'right', marginLeft: '4px' }}>
                  <Label circular color="green" empty />
                </span>
              )}

              {props.terms.includes('2020 Summer1') && (
                <span style={{ float: 'right', marginLeft: '4px' }}>
                  <Label circular color="orange" empty />
                </span>
              )}

              {props.terms.includes('2020 Spring') && (
                <span style={{ float: 'right', marginLeft: '4px' }}>
                  <Label circular color="teal" empty />
                </span>
              )}
            </div>
          }
          content={
            <div style={{ display: 'flex', flexDirection: 'column' }}>
              <h5 style={{ marginBottom: '4px' }}>Offered in:</h5>
              {props.terms.includes('2020 Fall') && (
                <div style={{ float: 'right' }}>
                  <Label circular color="yellow" empty />
                  <span style={{ marginLeft: '6px' }}>Fall 2020</span>
                </div>
              )}

              {props.terms.includes('2020 Summer2') && (
                <div style={{ float: 'right' }}>
                  <Label circular color="violet" empty />
                  <span style={{ marginLeft: '6px' }}>SS II 2020</span>
                </div>
              )}

              {props.terms.includes('2020 Summer10wk') && (
                <div style={{ float: 'right' }}>
                  <Label circular color="green" empty />
                  <span style={{ marginLeft: '6px' }}>SS 10wk 2020</span>
                </div>
              )}

              {props.terms.includes('2020 Summer1') && (
                <div style={{ float: 'right' }}>
                  <Label circular color="orange" empty />
                  <span style={{ marginLeft: '6px' }}>SS I 2020</span>
                </div>
              )}

              {props.terms.includes('2020 Spring') && (
                <div style={{ float: 'right' }}>
                  <Label circular color="teal" empty />
                  <span style={{ marginLeft: '6px' }}>Spring 2020</span>
                </div>
              )}
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
