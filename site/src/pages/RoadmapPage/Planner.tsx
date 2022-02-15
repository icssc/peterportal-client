import React, { FC, useEffect } from "react";
import "./Planner.scss";
import Header from "./Header";
import AddYearPopup from "./AddYearPopup";
import Year from "./Year";
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { selectYearPlans, setYearPlans, setInvalidCourses } from '../../store/slices/roadmapSlice';
import { useFirstRender } from "../../hooks/firstRenderer";
import { CourseIdentifier } from '../../types/types';

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
    let invalidCourses: CourseIdentifier[] = [];
    data.forEach((year, yi) => {
      year.quarters.forEach((quarter, qi) => {
        let taking: Set<string> = new Set(quarter.courses.map(course => course.department + ' ' + course.number));
        quarter.courses.forEach((course, ci) => {
          if (course.prerequisite_tree && !validateCourse(taken, JSON.parse(course.prerequisite_tree), taking, course.corequisite)) {
            console.log('invalid course', course.id);
            invalidCourses.push({
              yearIndex: yi,
              quarterIndex: qi,
              courseIndex: ci
            })
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

  const validateCourse = (taken: Set<string>, prerequisite: PrerequisiteNode, taking: Set<string>, corequisite: string): boolean => {
    // base case just a course
    if (typeof prerequisite === 'string') {
      // already taken prerequisite or is currently taking the corequisite
      return taken.has(prerequisite) || (corequisite.includes(prerequisite) && taking.has(prerequisite));
    }
    // has nested prerequisites
    else {
      // needs to satisfy all nested
      if (prerequisite.AND) {
        return prerequisite.AND.every(nested => validateCourse(taken, nested, taking, corequisite));
      }
      // only need to satisfy one nested
      else if (prerequisite.OR) {
        return prerequisite.OR.some(nested => validateCourse(taken, nested, taking, corequisite));
      }
      else {
        // should never reach here
        return false;
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
