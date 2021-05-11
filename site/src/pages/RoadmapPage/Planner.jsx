import React, { useState } from "react";
import "./Planner.scss";
import Header from "./Header.jsx";
import AddYearPopup from "./AddYearPopup.jsx";
import Year from "./Year.jsx";

const Planner = ({ handleAddYear, removeYear, state }) => {
  const [courseCount, setCourseCount] = useState(0);
  const [unitCount, setUnitCount] = useState(0);

  const addYearToPlanner = (year) => {
    handleAddYear(year);
  };
  let yearKeys = Array.from(Object.keys(state["year-plans"]));

  return (
    <div className="planner">
      <Header courseCount={courseCount} unitCount={unitCount} />
      <section className="years">
        {yearKeys.map((key) => {
          let year = state["year-plans"][key];
          return (
            <Year
              key={year.index}
              removeYear={removeYear}
              {...year}
              state={state}
            />
          );
        })}
      </section>
      <AddYearPopup addYearToPlanner={addYearToPlanner} />
    </div>
  );
};

export default Planner;
