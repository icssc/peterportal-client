import React from 'react'
import "./Course.scss";

const Course = ({ course }) => {

  return (
    <div className="course">
      <div className="name">{course.name}</div>
      <div className="title">{course.title}</div>
      <div className="units">
        {course.units} units
      </div>
    </div>
  );
}

export default Course