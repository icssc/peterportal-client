import React, { useState } from "react";
import "./PlannerPage.scss";
import Header from "./Header.jsx";
import AddYearPopup from "./AddYearPopup.jsx";
import YearModule from "./YearModule";

function PlannerPage({ yearPlans, handleAddYear, removeYear }) {
  const [courseCount, setCourseCount] = useState(0);
  const [unitCount, setUnitCount] = useState(0);
  const [popUp, setPopUp] = useState(false);

  const addYearToPlanner = (year) => {
    handleAddYear(year);
    setPopUp(false);
  };

  return (
    <div className="planner-page">
      <Header courseCount={courseCount} unitCount={unitCount} />
      <section className="year-modules">
        {yearPlans.map((year) => {
          return (
            <YearModule key={year.index} removeYear={removeYear} {...year} />
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

export default PlannerPage;
