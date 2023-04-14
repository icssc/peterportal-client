import React, { FC, useEffect } from "react";
import { useCookies } from 'react-cookie';
import axios from 'axios';
import "./Planner.scss";
import Header from "./Header";
import AddYearPopup from "./AddYearPopup";
import Year from "./Year";
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { selectYearPlans, setYearPlans, setInvalidCourses, setTransfers, addYear } from '../../store/slices/roadmapSlice';
import { useFirstRender } from "../../hooks/firstRenderer";
import { InvalidCourseData, SavedRoadmap, PlannerData, PlannerYearData, PlannerQuarterData, SavedPlannerData, SavedPlannerYearData, SavedPlannerQuarterData, BatchCourseData, MongoRoadmap } from '../../types/types';
import { searchAPIResults } from '../../helpers/util';

const Planner: FC = () => {
  const dispatch = useAppDispatch();
  const [cookies, setCookie] = useCookies(['user']);
  const isFirstRenderer = useFirstRender();
  const data = useAppSelector(selectYearPlans);
  const transfers = useAppSelector(state => state.roadmap.transfers);

  useEffect(() => {
    // if is first render, load from local storage
    if (isFirstRenderer) {
      loadRoadmap();
    }
    // validate planner every time something changes
    else {
      validatePlanner();
    }
  }, [data, transfers]);

  // remove all unecessary data to store into the database
  const collapsePlanner = (planner: PlannerData): SavedPlannerData => {
    let savedPlanner: SavedPlannerData = [];
    planner.forEach(year => {
      let savedYear: SavedPlannerYearData = { startYear: year.startYear, quarters: [] };
      year.quarters.forEach(quarter => {
        let savedQuarter: SavedPlannerQuarterData = { name: quarter.name, courses: [] };
        savedQuarter.courses = quarter.courses.map(course => course.id);
        savedYear.quarters.push(savedQuarter);
      })
      savedPlanner.push(savedYear);
    })
    return savedPlanner;
  }

  // query the lost information from collapsing
  const expandPlanner = async (savedPlanner: SavedPlannerData): Promise<PlannerData> => {
    return new Promise(async (resolve) => {
      let courses: string[] = [];
      // get all courses in the planner
      savedPlanner.forEach(year => year.quarters.forEach(quarter => { courses = courses.concat(quarter.courses) }))
      // get the course data for all courses
      let courseLookup: BatchCourseData = {};
      // only send request if there are courses
      if (courses.length > 0) {
        courseLookup = await searchAPIResults('courses', courses) as BatchCourseData;
      }
      let planner: PlannerData = [];
      savedPlanner.forEach(savedYear => {
        let year: PlannerYearData = { startYear: savedYear.startYear, quarters: [] };
        savedYear.quarters.forEach(savedQuarter => {
          let quarter: PlannerQuarterData = { name: savedQuarter.name, courses: [] };
          quarter.courses = savedQuarter.courses.map(course => courseLookup[course]);
          year.quarters.push(quarter);
        })
        planner.push(year);
      })
      console.log('EXPANDED PLANNER', planner)
      resolve(planner);
    })
  }

  const loadRoadmap = async () => {
    console.log('Loading Roadmaps...');
    let roadmap: SavedRoadmap = null!;
    let localRoadmap = localStorage.getItem('roadmap');
    // if logged in
    if (cookies.hasOwnProperty('user')) {
      // get data from account
      let request = await axios.get<MongoRoadmap>('/roadmap/get', { params: { id: cookies.user.id } });
      // if a roadmap is found
      if (!request.data.hasOwnProperty('error')) {
        roadmap = request.data.roadmap;
      }
    }
    // check local storage next
    if (!roadmap && localRoadmap) {
      roadmap = JSON.parse(localRoadmap);
    }
    // no saved planner
    if (!roadmap) {
      return;
    }

    // expand planner and set the state
    let planner = await expandPlanner(roadmap.planner);
    dispatch(setYearPlans(planner));
    dispatch(setTransfers(roadmap.transfers));
  }

  const saveRoadmap = () => {
    console.log('Saving Roadmaps...');
    let roadmap: SavedRoadmap = {
      planner: collapsePlanner(data),
      transfers: transfers
    };
    let savedAccount = false;
    // if logged in
    if (cookies.hasOwnProperty('user')) {
      // save data to account
      let mongoRoadmap: MongoRoadmap = { _id: cookies.user.id, roadmap: roadmap }
      axios.post('/roadmap', mongoRoadmap);
      savedAccount = true;
    }

    // save to local storage as well
    localStorage.setItem('roadmap', JSON.stringify(roadmap));

    if (savedAccount) {
      alert(`Roadmap saved under ${cookies.user.email}`);
    }
    else {
      alert('Roadmap saved locally! Login to save it to your account.');
    }
  }

  const calculatePlannerOverviewStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    // sum up all courses
    data.forEach(year => {
      year.quarters.forEach(quarter => {
        quarter.courses.forEach(course => {
          unitCount += course.units[0];
          courseCount += 1;
        })
      })
    })
    // add in transfer courses
    transfers.forEach(transfer => {
      // only count if has both name and units
      if (transfer.units && transfer.name) {
        unitCount += transfer.units;
        courseCount += 1;
      }
    });
    return { unitCount, courseCount };
  };

  const validatePlanner = () => {
    // store courses that have been taken
    let taken: Set<string> = new Set(transfers.map(transfer => transfer.name));
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
  //TODO: Support for Multiple Planner future implementation
  //  - Default year only added when a new planner is created

  const initializePlanner = () => {
    if (data.length == 0) {
      dispatch(addYear(
        {
          yearData: {
            startYear: new Date().getFullYear(),
            quarters: ['fall', 'winter', 'spring'].map(quarter => { return { name: quarter, courses: [] } })
          }
        }
      ))
    }
  
    return data.map((year, yearIndex) => {
      return (
        <Year
          key={yearIndex}
          yearIndex={yearIndex}
          data={year}
        />
      );
    })
  }

  let { unitCount, courseCount } = calculatePlannerOverviewStats();

  return (
    <div className="planner">
      <Header courseCount={courseCount} unitCount={unitCount} saveRoadmap={saveRoadmap} />
      <section className="years">
        {initializePlanner()}
      </section>
      <AddYearPopup placeholderYear={data.length === 0 ? new Date().getFullYear() : data[data.length - 1].startYear + 1} />
    </div>
  );
};
export default Planner;
