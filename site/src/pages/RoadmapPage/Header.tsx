import React, { FC } from "react";
import "./Header.scss";
import { Button, ButtonGroup } from "react-bootstrap";
import { ArrowLeftRight, Download } from "react-bootstrap-icons";
import html2canvas from 'html2canvas';
import { setShowTransfer } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import Transfer from './Transfer';

interface HeaderProps {
  courseCount: number;
  unitCount: number;
}

const Header: FC<HeaderProps> = ({ courseCount, unitCount }) => {
  const dispatch = useAppDispatch();

  return (
    <div className="header">
      <Transfer />
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
        <Button variant="light" className="header-btn" onClick={() => dispatch(setShowTransfer(true))}>
          Transfer Credits
          <ArrowLeftRight className="header-icon" />
        </Button>
      </ButtonGroup>
    </div>
  )
};

export default Header;
