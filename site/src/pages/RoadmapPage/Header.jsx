import React from "react";
import "./Header.scss";
import { Button, ButtonGroup } from "react-bootstrap";
import { ArrowCounterclockwise, Download } from "react-bootstrap-icons";
import html2canvas from 'html2canvas';

const Header = ({ courseCount, unitCount }) => {

  const handleExport = () => {
    html2canvas(document.getElementById('screenshot'), {
      scale: 1,
    }).then((canvas) => {
      const img = canvas.toDataURL('image/png');
      const lnk = document.createElement('a');
      lnk.download = 'PetersRoadmap.png';
      lnk.href = img;

      if (document.createEvent) {
          const e = document.createEvent('MouseEvents');
          //event.initMouseEvent(type, canBubble, cancelable, view, detail, screenX, screenY, clientX, clientY, ctrlKey, altKey, shiftKey, metaKey, button, relatedTarget);
          e.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
          lnk.dispatchEvent(e);
      } else if (lnk.fireEvent) {
          lnk.fireEvent('onclick');
      }
    });
  };

  return (
    <div className="header">
      <span id="planner-stats">
        Total: <span id="course-count">{courseCount}</span> courses,{" "}
        <span id="unit-count">{unitCount}</span> units
      </span>
      <span id="title">
        <h2>Peter's Roadmap</h2>
      </span>
      <ButtonGroup>
        <Button variant="light" className="header-btn">
          Undo
          <ArrowCounterclockwise className="header-icon" />
        </Button>
        <Button 
          variant="light" 
          className="header-btn"
          onClick={handleExport}
        >
          Export
          <Download className="header-icon" />
        </Button>
      </ButtonGroup>
    </div>
  )
};

export default Header;
