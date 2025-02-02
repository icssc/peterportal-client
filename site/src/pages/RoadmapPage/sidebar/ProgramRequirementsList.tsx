import './ProgramRequirementsList.scss';
import React, { FC, useCallback, useEffect, useState } from 'react';
import {
  checkCompletion,
  collapseSingletonRequirements,
  COMPLETE_ALL_TEXT,
  LOADING_COURSE_PLACEHOLDER,
} from '../../../helpers/courseRequirements';
import { CaretDownFill, CaretRightFill } from 'react-bootstrap-icons';
import { CourseNameAndInfo } from '../Course';
import { CourseGQLData } from '../../../types/types';
import trpc from '../../../trpc';
import { programRequirementsSortable } from '../../../helpers/sortable';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { useIsMobile } from '../../../helpers/util';
import { setActiveCourse, setActiveCourseLoading, setShowAddCourse } from '../../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { Spinner } from 'react-bootstrap';
import { ProgramRequirement, TypedProgramRequirement } from '@peterportal/types';

interface CourseTileProps {
  courseID: string;
  /** The timestamp at which the course data is requested to load */
  dragTimestamp?: number;
  taken?: boolean;
}

const CourseTile: FC<CourseTileProps> = ({ courseID, dragTimestamp = 0, taken }) => {
  const [courseData, setCourseData] = useState<string | CourseGQLData>(courseID);
  const [loading, setLoading] = useState(false);
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  const loadFullData = useCallback(async () => {
    if (typeof courseData !== 'string') return courseData;
    setLoading(true);
    const response = (await trpc.courses.get.query({ courseID })) as unknown as CourseGQLData;
    setCourseData(response);
    setLoading(false);
    return response;
  }, [courseData, courseID]);

  // Allows for dragging to trigger a data-load & setting active course
  useEffect(() => {
    if (!dragTimestamp) return;
    setLoading(true);
    dispatch(setActiveCourse(LOADING_COURSE_PLACEHOLDER));
    dispatch(setActiveCourseLoading(true));
    loadFullData().then((res) => {
      dispatch(setActiveCourse(res));
      dispatch(setActiveCourseLoading(false));
      setLoading(false);
    });
  }, [dispatch, dragTimestamp, loadFullData]);

  const handlePopoverStateChange = (open: boolean) => open && loadFullData();

  const insertCourseOnClick = async () => {
    setLoading(true);
    const fullData = await loadFullData();
    dispatch(setActiveCourse(fullData as CourseGQLData));
    dispatch(setShowAddCourse(true));
    setLoading(false);
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = isMobile ? tapProps : {};
  const className =
    `program-course-tile ${isMobile ? 'mobile' : ''} ` + `${loading ? 'loading' : ''} ${taken ? 'completed' : ''}`;

  return (
    <div className={className} {...tappableCourseProps}>
      <CourseNameAndInfo data={courseData} openPopoverLeft popupListener={handlePopoverStateChange} alwaysCollapse />
      {isMobile && loading && <Spinner animation="border" />}
    </div>
  );
};

const CourseList: FC<{ courses: string[]; takenCourseIDs: Set<string> }> = ({ courses, takenCourseIDs }) => {
  const isMobile = useIsMobile();
  const [timestamps, setTimestamps] = useState<number[]>(new Array(courses.length).fill(0));

  const setDraggedItem = async (event: SortableEvent) => {
    timestamps[event.oldIndex!] = Date.now();
    setTimestamps(timestamps.slice());
  };

  const courseIDs = courses.map((c) => ({ id: c }));
  return (
    <ReactSortable
      {...programRequirementsSortable}
      list={courseIDs}
      onStart={setDraggedItem}
      disabled={isMobile}
      className={'group-courses' + (isMobile ? ' disabled' : '')}
    >
      {courses.map((c, i) => (
        <CourseTile courseID={c} key={c} dragTimestamp={timestamps[i]} taken={takenCourseIDs.has(c)} />
      ))}
    </ReactSortable>
  );
};

interface GroupHeaderProps {
  title: string;
  open: boolean;
  setOpen: React.Dispatch<boolean>;
}
const GroupHeader: FC<GroupHeaderProps> = ({ title, open, setOpen }) => {
  const className = `group-header ${open ? 'open' : ''}`;
  return (
    <button className={className} onClick={() => setOpen(!open)}>
      {open ? <CaretDownFill /> : <CaretRightFill />}
      <b>{title}</b>
    </button>
  );
};

interface IndividualRequirementProps {
  data: TypedProgramRequirement<'Course' | 'Unit'>;
  takenCourseIDs: Set<string>;
}

const CourseRequirement: FC<IndividualRequirementProps> = ({ data, takenCourseIDs }) => {
  const [open, setOpen] = useState(false);

  let label: string | number;
  if ('courseCount' in data) {
    label = data.courseCount === data.courses.length ? 'all' : data.courseCount;
  } else {
    label = data.unitCount + ' units';
  }
  const showLabel = data.courses.length > 1 && data.label !== COMPLETE_ALL_TEXT;
  const complete = checkCompletion(takenCourseIDs, data).done;
  const className = `group-requirement ${complete ? 'completed' : ''}`;

  return (
    <div className={className}>
      <GroupHeader title={data.label} open={open} setOpen={setOpen} />
      {open && showLabel && (
        <p>
          <b>Complete {label} of the following:</b>
        </p>
      )}
      {open && <CourseList courses={data.courses} takenCourseIDs={takenCourseIDs} />}
    </div>
  );
};

const GroupedCourseRequirement: FC<IndividualRequirementProps> = ({ data, takenCourseIDs }) => {
  const complete = checkCompletion(takenCourseIDs, data).done;
  const className = `course-requirement ${complete ? 'completed' : ''}`;

  return (
    <div className={className}>
      <CourseList courses={data.courses} takenCourseIDs={takenCourseIDs} />
    </div>
  );
};

interface GroupRequirementProps {
  data: TypedProgramRequirement<'Group'>;
  takenCourseIDs: Set<string>;
}
const GroupRequirement: FC<GroupRequirementProps> = ({ data, takenCourseIDs }) => {
  const [open, setOpen] = useState(false);
  const complete = checkCompletion(takenCourseIDs, data).done;
  const className = `group-requirement ${complete ? 'completed' : ''}`;

  return (
    <div className={className}>
      <GroupHeader title={data.label} open={open} setOpen={setOpen} />
      {open && (
        <p>
          Complete <b>{data.requirementCount}</b> of the following series:
        </p>
      )}
      {open &&
        data.requirements.map((r, i) => (
          <React.Fragment key={i}>
            <p>
              <b>{r.label}</b>
            </p>
            <ProgramRequirementDisplay requirement={r} nested takenCourseIDs={takenCourseIDs} />
          </React.Fragment>
        ))}
    </div>
  );
};

interface ProgramRequirementDisplayProps {
  requirement: ProgramRequirement;
  nested?: boolean;
  takenCourseIDs: Set<string>;
}
const ProgramRequirementDisplay: FC<ProgramRequirementDisplayProps> = ({ requirement, nested, takenCourseIDs }) => {
  switch (requirement.requirementType) {
    case 'Unit':
    case 'Course': {
      const DisplayComponent = nested ? GroupedCourseRequirement : CourseRequirement;
      return <DisplayComponent data={requirement} takenCourseIDs={takenCourseIDs} />;
    }
    case 'Group':
      return <GroupRequirement data={requirement} takenCourseIDs={takenCourseIDs} />;
  }
};

interface RequireCourseListProps {
  requirements: ProgramRequirement[];
}

const ProgramRequirementsList: FC<RequireCourseListProps> = ({ requirements }) => {
  const collapsedRequirements = collapseSingletonRequirements(requirements);
  const roadmapTransfers = useAppSelector((state) => state.roadmap.transfers);
  const roadmapPlans = useAppSelector((state) => state.roadmap.plans);
  const roadmapPlanIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const yearPlans = roadmapPlans[roadmapPlanIndex].content.yearPlans;

  const roadmapCourseIDList = yearPlans
    .flatMap((year) => year.quarters)
    .flatMap((quarter) => quarter.courses)
    .flatMap((course) => course.id)
    .concat(roadmapTransfers.map((t) => t.name.replace(/\s/g, '')));
  const takenCourseSet = new Set(roadmapCourseIDList);

  return (
    <div className="program-requirements">
      {/* key is ok because we don't reorder these */}
      {collapsedRequirements.map((r, i) => (
        <ProgramRequirementDisplay requirement={r} key={i} takenCourseIDs={takenCourseSet} />
      ))}
    </div>
  );
};

export default ProgramRequirementsList;
