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

  const buildQuarterStats = (quarter, yearPlan) => {
    let quarterKey = yearPlan.index + "-" + quarter;
    let unitCount = 0;
    let courseCount = 0;
    if (quarterKey in yearPlan) {
      let courses = yearPlan[quarterKey];
      for (let course of courses) {
        unitCount += course.units;
        courseCount += 1;
      }
    }
    return [unitCount, courseCount];
  };

  const buildYearStats = (yearPlan) => {
    let fallStats = buildQuarterStats("fall", yearPlan);
    let winterStats = buildQuarterStats("winter", yearPlan);
    let springStats = buildQuarterStats("spring", yearPlan);
    let yearStats = {
      fall: fallStats,
      winter: winterStats,
      spring: springStats,
    };
    return yearStats;
  };

  const buildPlannerStats = () => {
    let plannerStats = {};
    for (let yearKey of yearKeys) {
      let year = state["year-plans"][yearKey];
      plannerStats[yearKey] = buildYearStats(year);
    }
    return plannerStats;
  };

  const calculatePlannerOverviewStats = (plannerStats) => {
    let unitCount = 0;
    let courseCount = 0;
    let yearKeys = Array.from(Object.keys(plannerStats));
    for (let yearKey of yearKeys) {
      let quarterKeys = Array.from(Object.keys(plannerStats[yearKey]));
      for (let quarterKey of quarterKeys) {
        unitCount += plannerStats[yearKey][quarterKey][0];
        courseCount += plannerStats[yearKey][quarterKey][1];
      }
    }
    return [unitCount, courseCount];
  };

  let plannerStats = buildPlannerStats();
  let overallStats = calculatePlannerOverviewStats(plannerStats);

  return (
    <div className="planner">
      <Header courseCount={overallStats[1]} unitCount={overallStats[0]} />
      <section className="years">
        {yearKeys.map((key) => {
          let year = state["year-plans"][key];
          return (
            <Year
              key={year.index}
              removeYear={removeYear}
              {...year}
              state={state}
              plannerStats={plannerStats}
            />
          );
        })}
      </section>
      <AddYearPopup addYearToPlanner={addYearToPlanner} />
    </div>
  );
};

export default Planner;
