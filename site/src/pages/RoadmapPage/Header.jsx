import React from "react";
import "./Header.scss";
import { Button } from "semantic-ui-react";

const Header = ({ courseCount, unitCount }) => (
  <div className="header">
    <span id="planner-stats">
      Total: <span id="course-count">{courseCount}</span> courses,{" "}
      <span id="unit-count">{unitCount}</span> units
    </span>
    <span id="title">
      <h2>Peter's Roadmap</h2>
    </span>
    <Button.Group>
      <Button className="header-btn" icon="undo" content="Undo" />
      <Button className="header-btn" icon="download" content="Export" />
    </Button.Group>
  </div>
);

export default Header;
