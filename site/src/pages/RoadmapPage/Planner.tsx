import { FC, useEffect, useState } from 'react';
import { useCookies } from 'react-cookie';
import axios from 'axios';
import './Planner.scss';
import Header from './Header';
import AddYearPopup from './AddYearPopup';
import Year from './Year';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import {
  selectYearPlans,
  setYearPlans,
  setInvalidCourses,
  setTransfers,
  setUnsavedChanges,
  setCoursebag,
} from '../../store/slices/roadmapSlice';
import { useFirstRender } from '../../hooks/firstRenderer';
import {
  InvalidCourseData,
  SavedRoadmap,
  PlannerData,
  PlannerYearData,
  PlannerQuarterData,
  SavedPlannerData,
  SavedPlannerYearData,
  SavedPlannerQuarterData,
  BatchCourseData,
  MongoRoadmap,
  Coursebag,
  CourseGQLData,
} from '../../types/types';
import { searchAPIResult, searchAPIResults } from '../../helpers/util';
import { Prerequisite, PrerequisiteTree } from 'peterportal-api-next-types';
import { defaultYear, normalizeQuarterName } from '../../helpers/planner';
import ImportTranscriptPopup from './ImportTranscriptPopup';

const Planner: FC = () => {
  const dispatch = useAppDispatch();
  const [cookies] = useCookies(['user']);
  const isFirstRenderer = useFirstRender();
  const data = useAppSelector(selectYearPlans);
  const coursebag = useAppSelector((state) => state.roadmap.coursebag);
  const transfers = useAppSelector((state) => state.roadmap.transfers);

  const [missingPrerequisites, setMissingPrerequisites] = useState(new Set<string>());

  useEffect(() => {
    // stringify current roadmap

    const roadmapStr = JSON.stringify({
      planner: collapsePlanner(data),
      transfers: transfers,
    });

    // stringified value of an empty roadmap
    const emptyRoadmap = JSON.stringify({
      planner: [defaultYear()],
      transfers: [],
    } as SavedRoadmap);

    // if first render and current roadmap is empty, load from local storage
    if (isFirstRenderer && roadmapStr === emptyRoadmap) {
      loadRoadmap();
    }
    // validate planner every time something changes
    else {
      validatePlanner();

      // check current roadmap against last-saved roadmap in local storage
      // if they are different, mark changes as unsaved to enable alert on page leave
      dispatch(setUnsavedChanges(localStorage.getItem('roadmap') !== roadmapStr));
    }
  }, [data, transfers]);

  // remove all unecessary data to store into the database
  const collapsePlanner = (planner: PlannerData): SavedPlannerData => {
    const savedPlanner: SavedPlannerData = [];
    planner.forEach((year) => {
      const savedYear: SavedPlannerYearData = { startYear: year.startYear, name: year.name, quarters: [] };
      year.quarters.forEach((quarter) => {
        const savedQuarter: SavedPlannerQuarterData = { name: quarter.name, courses: [] };
        savedQuarter.courses = quarter.courses.map((course) => course.id);
        savedYear.quarters.push(savedQuarter);
      });
      savedPlanner.push(savedYear);
    });
    return savedPlanner;
  };

  // query the lost information from collapsing
  const expandPlanner = async (savedPlanner: SavedPlannerData): Promise<PlannerData> => {
    let courses: string[] = [];
    // get all courses in the planner
    savedPlanner.forEach((year) =>
      year.quarters.forEach((quarter) => {
        courses = courses.concat(quarter.courses);
      }),
    );
    // get the course data for all courses
    let courseLookup: BatchCourseData = {};
    // only send request if there are courses
    if (courses.length > 0) {
      courseLookup = (await searchAPIResults('courses', courses)) as BatchCourseData;
    }

    return new Promise((resolve) => {
      const planner: PlannerData = [];
      savedPlanner.forEach((savedYear) => {
        const year: PlannerYearData = { startYear: savedYear.startYear, name: savedYear.name, quarters: [] };
        savedYear.quarters.forEach((savedQuarter) => {
          const transformedName = normalizeQuarterName(savedQuarter.name);
          const quarter: PlannerQuarterData = { name: transformedName, courses: [] };
          quarter.courses = savedQuarter.courses.map((course) => courseLookup[course]);
          year.quarters.push(quarter);
        });
        planner.push(year);
      });
      resolve(planner);
    });
  };

  const loadRoadmap = async () => {
    let roadmap: SavedRoadmap = null!;
    const coursebag: Coursebag = [];
    const localRoadmap = localStorage.getItem('roadmap');
    // if logged in
    if (cookies.user !== undefined) {
      // get data from account
      const request = await axios.get<MongoRoadmap>('/api/roadmap/get', { params: { id: cookies.user.id } });
      // if a roadmap is found
      if (request.data.roadmap !== undefined) {
        roadmap = request.data.roadmap;
      }
      const courses = (await Promise.all(
        request.data.coursebag.map((course) => searchAPIResult('course', course)),
      )) as CourseGQLData[];
      coursebag.push(...courses);
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
    const planner = await expandPlanner(roadmap.planner);
    dispatch(setYearPlans(planner));
    dispatch(setTransfers(roadmap.transfers));
    dispatch(setCoursebag(coursebag));
  };

  const saveRoadmap = () => {
    const roadmap: SavedRoadmap = {
      planner: collapsePlanner(data),
      transfers: transfers,
    };
    const collapsedCoursebag = coursebag.map((course) => course.id);
    let savedAccount = false;
    // if logged in
    if (cookies.user !== undefined) {
      // save data to account
      const mongoRoadmap: MongoRoadmap = { _id: cookies.user.id, roadmap: roadmap, coursebag: collapsedCoursebag };
      axios.post('/api/roadmap', mongoRoadmap);
      savedAccount = true;
    }

    // save to local storage as well
    localStorage.setItem('roadmap', JSON.stringify(roadmap));

    // mark changes as saved to bypass alert on page leave
    dispatch(setUnsavedChanges(false));

    if (savedAccount) {
      alert(`Roadmap saved under ${cookies.user.email}`);
    } else {
      alert('Roadmap saved locally! Login to save it to your account.');
    }
  };

  const calculatePlannerOverviewStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    // sum up all courses
    data.forEach((year) => {
      year.quarters.forEach((quarter) => {
        quarter.courses.forEach((course) => {
          unitCount += course.minUnits;
          courseCount += 1;
        });
      });
    });
    // add in transfer courses
    transfers.forEach((transfer) => {
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
    const taken: Set<string> = new Set(transfers.map((transfer) => transfer.name));
    const invalidCourses: InvalidCourseData[] = [];
    const missing = new Set<string>();
    data.forEach((year, yi) => {
      year.quarters.forEach((quarter, qi) => {
        const taking: Set<string> = new Set(
          quarter.courses.map((course) => course.department + ' ' + course.courseNumber),
        );
        quarter.courses.forEach((course, ci) => {
          // if has prerequisite
          if (course.prerequisiteTree) {
            const required = validateCourse(taken, course.prerequisiteTree, taking, course.corequisites);
            // prerequisite not fulfilled, has some required classes to take
            if (required.size > 0) {
              invalidCourses.push({
                location: {
                  yearIndex: yi,
                  quarterIndex: qi,
                  courseIndex: ci,
                },
                required: Array.from(required),
              });

              required.forEach((course) => {
                missing.add(course);
              });
            }
          }
        });
        // after the quarter is over, add the courses into taken
        taking.forEach((course) => taken.add(course));
      });
    });

    // set missing courses
    setMissingPrerequisites(missing);

    // set the invalid courses
    dispatch(setInvalidCourses(invalidCourses));
  };

  type PrerequisiteNode = Prerequisite | PrerequisiteTree;

  // returns set of courses that need to be taken to fulfill requirements
  const validateCourse = (
    taken: Set<string>,
    prerequisite: PrerequisiteNode,
    taking: Set<string>,
    corequisite: string,
  ): Set<string> => {
    // base case just a course
    if ('prereqType' in prerequisite) {
      const id = prerequisite?.courseId ?? prerequisite?.examName ?? '';
      // already taken prerequisite or is currently taking the corequisite
      if (taken.has(id) || (corequisite.includes(id) && taking.has(id))) {
        return new Set();
      }
      // need to take this prerequisite still
      else {
        return new Set([id]);
      }
    }
    // has nested prerequisites
    else {
      // needs to satisfy all nested
      if (prerequisite.AND) {
        const required: Set<string> = new Set();
        prerequisite.AND.forEach((nested) => {
          // combine all the courses that are required
          validateCourse(taken, nested, taking, corequisite).forEach((course) => required.add(course));
        });
        return required;
      }
      // only need to satisfy one nested
      else if (prerequisite.OR) {
        const required: Set<string> = new Set();
        let satisfied = false;
        prerequisite.OR.forEach((nested) => {
          // combine all the courses that are required
          const courses = validateCourse(taken, nested, taking, corequisite);
          // if one is satisfied, no other courses are required
          if (courses.size == 0) {
            satisfied = true;
            return;
          }
          courses.forEach((course) => required.add(course));
        });
        return satisfied ? new Set() : required;
      } else {
        // should never reach here
        return new Set();
      }
    }
  };

  const { unitCount, courseCount } = calculatePlannerOverviewStats();

  return (
    <div className="planner">
      <Header
        courseCount={courseCount}
        unitCount={unitCount}
        saveRoadmap={saveRoadmap}
        missingPrerequisites={missingPrerequisites}
      />
      <section className="years">
        {data.map((year, yearIndex) => {
          return <Year key={yearIndex} yearIndex={yearIndex} data={year} />;
        })}
      </section>
      <AddYearPopup
        placeholderName={'Year ' + (data.length + 1)}
        placeholderYear={data.length === 0 ? new Date().getFullYear() : data[data.length - 1].startYear + 1}
      />
      <ImportTranscriptPopup />
    </div>
  );
};
export default Planner;
