import React, { useState } from "react";
import "./PlannerPage.scss";
import Header from "./Header.jsx";
import AddYearPopup from "./AddYearPopup.jsx";
import YearModule from "./YearModule";

function PlannerPage() {
  const [courseCount, setCourseCount] = useState(0);
  const [unitCount, setUnitCount] = useState(0);
  const [popUp, setPopUp] = useState(false);
  const [yearPlans, setYearPlans] = useState([]);

  const handleAddYear = (year) => {
    const newIndex =
      yearPlans.length !== 0 ? yearPlans[yearPlans.length - 1].index + 1 : 1;
    const newYear = {
      index: newIndex,
      startYear: parseInt(year),
      courses: 0,
      units: 0,
    };
    setYearPlans([...yearPlans, newYear]);
    setPopUp(false);
  };

  const removeYear = (year) => {
    const filteredPlans = yearPlans.filter((plan) => plan.index !== year);
    setYearPlans(filteredPlans);
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
        handleAddYear={handleAddYear}
        setPopUp={setPopUp}
        popUp={popUp}
      />
    </div>
  );
}

export default PlannerPage;
