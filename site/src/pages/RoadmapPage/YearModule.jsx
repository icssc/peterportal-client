import React, { useState } from "react";
import "./YearModule.scss";
import { Button, Icon, Popup, Grid } from "semantic-ui-react";

function YearModule({ index, startYear, courses, units, removeYear }) {
  const [showContent, setShowContent] = useState(false);

  return (
    <div className="year-module">
      <Button
        className="accordion"
        onClick={() => {
          setShowContent(!showContent);
        }}
      >
        <span className="accordion-title">
          <span id="year-title">
            <Icon
              id="accordion-icon"
              name={showContent ? "triangle down" : "triangle right"}
            />
            <span id="year-number">Year {index} </span>
            <span id="year-range">
              ({startYear} - {startYear + 1})
            </span>
          </span>
          <span id="year-stats">
            <span id="course-count">{courses}</span> courses,{" "}
            <span id="unit-count">{units}</span> units
          </span>
        </span>
      </Button>
      <Popup
        content={
          <div>
            <Button className="year-settings-btn">Edit Year</Button>
            <Button
              className="year-settings-btn"
              id="remove-btn"
              onClick={() => {
                removeYear(index);
              }}
            >
              Remove
            </Button>
          </div>
        }
        className="year-settings-popup"
        on="click"
        trigger={<Button className="edit-btn" icon="ellipsis horizontal" />}
        position="bottom center"
      />
      {showContent && (
        <div className="accordion-content">Placeholder content</div>
      )}
    </div>
  );
}

export default YearModule;
