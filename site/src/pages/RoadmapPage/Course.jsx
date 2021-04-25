import React from "react";
import "./Course.scss";

const Course = ({ name, title, units }) => {
  return (
    <div className="course">
      <div className="name">{name}</div>
      <div className="title">{title}</div>
      <div className="units">{units} units</div>
    </div>
  );
};

export default Course;
