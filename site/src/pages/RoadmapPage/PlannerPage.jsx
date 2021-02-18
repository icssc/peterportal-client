import React, { useState } from "react";
import "./PlannerPage.scss";
import Header from "./Header.jsx";
import AddYearPopup from "./AddYearPopup.jsx";

function PlannerPage() {
  const [courseCount, setCourseCount] = useState(0);
  const [unitCount, setUnitCount] = useState(0);
  const [popUp, setPopUp] = useState(false);

  const handleAddYear = (year) => {
    console.log("Adding year " + year);
    setPopUp(false);
  };

  return (
    <div className="planner-page">
      <Header courseCount={courseCount} unitCount={unitCount} />
      <h1>Planner Page</h1>
      <AddYearPopup
        handleAddYear={handleAddYear}
        setPopUp={setPopUp}
        popUp={popUp}
      />
    </div>
  );
}

export default PlannerPage;
