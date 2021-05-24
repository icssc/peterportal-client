import React from "react";
import "./Header.scss";
import { Button, ButtonGroup } from "react-bootstrap";
import { ArrowCounterclockwise, Download } from "react-bootstrap-icons";

const Header = ({ courseCount, unitCount }) => (
  <div className="header">
    <span id="planner-stats">
      Total: <span id="course-count">{courseCount}</span>{" "}
      {courseCount === 1 ? "course" : "courses"},{" "}
      <span id="unit-count">{unitCount}</span>{" "}
      {unitCount === 1 ? "unit" : "units"}
    </span>
    <span id="title">
      <h2>Peter's Roadmap</h2>
    </span>
    <ButtonGroup>
      <Button variant="light" className="header-btn">
        Undo
        <ArrowCounterclockwise className="header-icon" />
      </Button>
      <Button variant="light" className="header-btn">
        Export
        <Download className="header-icon" />
      </Button>
    </ButtonGroup>
  </div>
);

export default Header;
