import React from 'react';
import {Grid, Divider} from "semantic-ui-react";
import './AdditionalInfo.scss';

function AdditionalInfo(props) {
    return (
        <div>
        <Grid.Row className="feature-label">
          <h2><span role="img" aria-label="book">📘</span> Course Information</h2>
          <Divider />
        </Grid.Row>
        
        <Grid.Row id="course_addl-info">
          <Grid.Column width={10} id="course_desc-container">
            <div className="course_desc-field-container">
              <p style={{ marginBottom: "6px" }}>
                <b>Course Description</b>
              </p>
              <p>{props.description}</p>
            </div>
            {props.restriction !== "" && (
              <div className="course_desc-field-container">
                <p style={{ marginBottom: "6px" }}>
                  <b>Restriction</b>
                </p>
                <p>{props.restriction}</p>
              </div>
            )}

            {props.repeatability !== "" && (
              <div className="course_desc-field-container">
                <p style={{ marginBottom: "6px" }}>
                  <b>Repeatability</b>
                </p>
                <p>{props.restriction}</p>
              </div>
            )}

            {props.overlap !== "" && (
              <p>
                <b>Overlaps with </b>
                {props.overlap}
              </p>
            )}

            {props.concurrent !== "" && (
              <p>
                <b>Concurrent with </b>
                {props.concurrent}
              </p>
            )}
          </Grid.Column>

          <Grid.Column width={2} id="course_ge-info">
            <div className="course_ge-info-container">
              {props.ge_list.length > 0 && (
                <p style={{ marginBottom: "6px" }}>
                  <b>GE Criteria</b>
                </p>
              )}

              {props.ge_list.map((value, index) => {
                return (
                  <p className="list-item" key={index}>
                    {value}
                  </p>
                );
              })}
            </div>

            <div className="course_ge-info-container">
              
              {Object.keys(props.professor_history).length > 0 && (
                <p style={{ marginBottom: "6px" }}>
                  <b>Instructor History</b>
                </p>
              )}

              {Object.keys(props.professor_history).map((key, index) => {
                return (
                  <p className="list-item" key={index}>
                    <a
                      href={"/professor/" + key}
                      className="list-item"
                    >
                      {props.professor_history[key]}
                    </a>
                  </p>
                );
              })}
            </div>
          </Grid.Column>
        </Grid.Row>
      </div>
    );
}

export default AdditionalInfo;