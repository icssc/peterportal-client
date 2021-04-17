import React, { useState } from "react";
import "./Planner.scss";
import Header from "./Header.jsx";
import AddYearPopup from "./AddYearPopup.jsx";
import Year from "./Year.jsx";

const Planner = ({ yearPlans, handleAddYear, removeYear, state }) => {
  const [courseCount, setCourseCount] = useState(0);
  const [unitCount, setUnitCount] = useState(0);
  const [popUp, setPopUp] = useState(false);

  const addYearToPlanner = (year) => {
    handleAddYear(year);
    setPopUp(false);
  };

  return (
    <div className="planner">
      <Header courseCount={courseCount} unitCount={unitCount} />
      <section className="years">
        {yearPlans.map((year) => {
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
      <AddYearPopup
        addYearToPlanner={addYearToPlanner}
        setPopUp={setPopUp}
        popUp={popUp}
      />
    </div>
  );
}

export default Planner;
