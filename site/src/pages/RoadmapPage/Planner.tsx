import React, { FC, useEffect } from "react";
import "./Planner.scss";
import Header from "./Header";
import AddYearPopup from "./AddYearPopup";
import Year from "./Year";
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { selectYearPlans, setYearPlans } from '../../store/slices/roadmapSlice';
import { useFirstRender } from "../../hooks/firstRenderer";

const Planner: FC = () => {
  const dispatch = useAppDispatch();
  const isFirstRenderer = useFirstRender();
  const data = useAppSelector(selectYearPlans);

  useEffect(() => {
    // if is first render, load from local storage
    if (isFirstRenderer) {
      console.log('FR', data);
      let localState = localStorage.getItem('roadmapState');
      if (localState) {
        dispatch(setYearPlans(JSON.parse(localState)))
      }
    }
    // constantly update local storage
    else {
      console.log('NR', data);
      localStorage.setItem('roadmapState', JSON.stringify(data));
    }
  }, [data]);

  const calculatePlannerOverviewStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    data.forEach(year => {
      year.quarters.forEach(quarter => {
        quarter.courses.forEach(course => {
          unitCount += course.units[0];
          courseCount += 1;
        })
      })
    })
    return { unitCount, courseCount };
  };

  let { unitCount, courseCount } = calculatePlannerOverviewStats();

  return (
    <div className="planner">
      <Header courseCount={courseCount} unitCount={unitCount} />
      <section className="years">
        {data.map((year, yearIndex) => {
          return (
            <Year
              key={yearIndex}
              yearIndex={yearIndex}
              data={year}
            />
          );
        })}
      </section>
      <AddYearPopup placeholderYear={data.length === 0 ? new Date().getFullYear() : data[data.length - 1].startYear + 1} />
    </div>
  );
};

export default Planner;
