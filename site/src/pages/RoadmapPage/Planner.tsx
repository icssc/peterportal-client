import React, { FC, useEffect } from "react";
import "./Planner.scss";
import Header from "./Header";
import AddYearPopup from "./AddYearPopup";
import Year from "./Year";
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { selectYearPlans, setYearPlans, setInvalidCourses } from '../../store/slices/roadmapSlice';
import { useFirstRender } from "../../hooks/firstRenderer";
import { InvalidCourseData } from '../../types/types';

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
    // constantly update local storage and validate planner
    else {
      console.log('NR', data);
      localStorage.setItem('roadmapState', JSON.stringify(data));
      validatePlanner();
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

  const validatePlanner = () => {
    // store courses that have been taken
    let taken: Set<string> = new Set();
    let invalidCourses: InvalidCourseData[] = [];
    data.forEach((year, yi) => {
      year.quarters.forEach((quarter, qi) => {
        let taking: Set<string> = new Set(quarter.courses.map(course => course.department + ' ' + course.number));
        quarter.courses.forEach((course, ci) => {
          // if has prerequisite
          if (course.prerequisite_tree) {
            let required = validateCourse(taken, JSON.parse(course.prerequisite_tree), taking, course.corequisite);
            // prerequisite not fulfilled, has some required classes to take
            if (required.size > 0) {
              console.log('invalid course', course.id);
              invalidCourses.push({
                location: {
                  yearIndex: yi,
                  quarterIndex: qi,
                  courseIndex: ci
                },
                required: Array.from(required)
              })
            }
          }
        })
        // after the quarter is over, add the courses into taken
        taking.forEach(course => taken.add(course));
      })
    })
    // set the invalid courses
    dispatch(setInvalidCourses(invalidCourses));
  }

  type PrerequisiteNode = NestedPrerequisiteNode | string;
  interface NestedPrerequisiteNode {
    AND?: PrerequisiteNode[];
    OR?: PrerequisiteNode[];
  }

  // returns set of courses that need to be taken to fulfill requirements
  const validateCourse = (taken: Set<string>, prerequisite: PrerequisiteNode, taking: Set<string>, corequisite: string): Set<string> => {
    // base case just a course
    if (typeof prerequisite === 'string') {
      // already taken prerequisite or is currently taking the corequisite
      if (taken.has(prerequisite) || (corequisite.includes(prerequisite) && taking.has(prerequisite))) {
        return new Set();
      }
      // need to take this prerequisite still
      else {
        return new Set([prerequisite]);
      }
    }
    // has nested prerequisites
    else {
      // needs to satisfy all nested
      if (prerequisite.AND) {
        let required: Set<string> = new Set();
        prerequisite.AND.forEach(nested => {
          // combine all the courses that are required
          validateCourse(taken, nested, taking, corequisite)
            .forEach(course => required.add(course));
        })
        return required;
      }
      // only need to satisfy one nested
      else if (prerequisite.OR) {
        let required: Set<string> = new Set();
        let satisfied = false;
        prerequisite.OR.forEach(nested => {
          // combine all the courses that are required
          let courses = validateCourse(taken, nested, taking, corequisite)
          // if one is satisfied, no other courses are required
          if (courses.size == 0) {
            satisfied = true;
            return;
          }
          courses.forEach(course => required.add(course));
      })
      return satisfied ? new Set() : required;
    }
      else {
  // should never reach here
  return new Set();
}
    }
  }

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
