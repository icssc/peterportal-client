import './ProgramRequirementsList.scss';
import React, { FC, useCallback, useEffect, useState } from 'react';
import {
  checkCompletion,
  collapseSingletonRequirements,
  COMPLETE_ALL_TEXT,
  CompletedCourseSet,
  flattenSingletonGroups,
  LOADING_COURSE_PLACEHOLDER,
} from '../../../helpers/courseRequirements';
import { CaretDownFill, CaretRightFill } from 'react-bootstrap-icons';
import { CourseNameAndInfo } from '../Course';
import { CourseGQLData } from '../../../types/types';
import trpc from '../../../trpc';
import { programRequirementsSortable } from '../../../helpers/sortable';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { useIsMobile } from '../../../helpers/util';
import {
  setActiveCourse,
  setActiveCourseLoading,
  setActiveMissingPrerequisites,
  setShowAddCourse,
} from '../../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { Spinner } from 'react-bootstrap';
import { ProgramRequirement, TypedProgramRequirement } from '@peterportal/types';
import { setGroupExpanded } from '../../../store/slices/courseRequirementsSlice';
import { getMissingPrerequisites } from '../../../helpers/planner';
import { useClearedCourses } from '../../../hooks/planner';
import { useTransferredCredits } from '../../../hooks/transferCredits';

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
  const clearedCourses = useClearedCourses();
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
    const missingPrerequisites = getMissingPrerequisites(clearedCourses, fullData);
    dispatch(setActiveCourse(fullData as CourseGQLData));
    dispatch(setActiveMissingPrerequisites(missingPrerequisites));
    dispatch(setShowAddCourse(true));
    setLoading(false);
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = isMobile ? tapProps : {};
  const className = `program-course-tile${isMobile ? ' mobile' : ''}${loading ? ' loading' : ''}${taken ? ' completed' : ''}`;
  let fontSize: string | undefined;

  if (courseID.length > 10) {
    const charsExtra = courseID.length - 10;
    const computedSize = 13 - charsExtra;
    fontSize = computedSize + 'px';
  }

  return (
    <div className={className} {...tappableCourseProps} style={{ fontSize }}>
      <CourseNameAndInfo data={courseData} openPopoverLeft popupListener={handlePopoverStateChange} alwaysCollapse />
      {isMobile && loading && <Spinner animation="border" />}
    </div>
  );
};

const CourseList: FC<{ courses: string[]; takenCourseIDs: CompletedCourseSet }> = ({ courses, takenCourseIDs }) => {
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
        <CourseTile courseID={c} key={c} dragTimestamp={timestamps[i]} taken={c in takenCourseIDs} />
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

interface CourseRequirementProps {
  data: TypedProgramRequirement<'Course' | 'Unit'>;
  takenCourseIDs: CompletedCourseSet;
  storeKey: string;
}

const CourseRequirement: FC<CourseRequirementProps> = ({ data, takenCourseIDs, storeKey }) => {
  const dispatch = useAppDispatch();

  const complete = checkCompletion(takenCourseIDs, data).done;

  const open = useAppSelector((state) => state.courseRequirements.expandedGroups[storeKey] ?? false);

  const setOpen = (isOpen: boolean) => {
    dispatch(setGroupExpanded({ storeKey: storeKey, expanded: isOpen }));
  };

  let label: string | number;
  if ('courseCount' in data) {
    label = data.courseCount === data.courses.length ? 'all' : data.courseCount;
  } else {
    label = data.unitCount + ' units';
  }
  const showLabel = data.courses.length > 1 && data.label !== COMPLETE_ALL_TEXT;
  const className = `group-requirement${complete ? ' completed' : ''}`;

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

interface GroupedCourseRequirementProps {
  data: TypedProgramRequirement<'Course' | 'Unit'>;
  takenCourseIDs: CompletedCourseSet;
}

const GroupedCourseRequirement: FC<GroupedCourseRequirementProps> = ({ data, takenCourseIDs }) => {
  const complete = checkCompletion(takenCourseIDs, data).done;
  const className = `course-requirement${complete ? ' completed' : ''}`;

  return (
    <>
      <div className={className}>
        <p>
          <b>{data.label}</b>
        </p>
        <CourseList courses={data.courses} takenCourseIDs={takenCourseIDs} />
      </div>
    </>
  );
};

interface GroupRequirementProps {
  data: TypedProgramRequirement<'Group'>;
  takenCourseIDs: CompletedCourseSet;
  storeKey: string;
}
const GroupRequirement: FC<GroupRequirementProps> = ({ data, takenCourseIDs, storeKey }) => {
  const dispatch = useAppDispatch();

  const complete = checkCompletion(takenCourseIDs, data).done;

  const open = useAppSelector((state) => state.courseRequirements.expandedGroups[storeKey] ?? false);

  const setOpen = (isOpen: boolean) => {
    dispatch(setGroupExpanded({ storeKey: storeKey, expanded: isOpen }));
  };

  const className = `group-requirement${complete ? ' completed' : ''}`;

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
          <ProgramRequirementDisplay
            key={i}
            storeKey={`${storeKey}-${i}`}
            requirement={r}
            nested
            takenCourseIDs={takenCourseIDs}
          />
        ))}
    </div>
  );
};

interface ProgramRequirementDisplayProps {
  requirement: ProgramRequirement;
  nested?: boolean;
  takenCourseIDs: CompletedCourseSet;
  storeKey: string;
}
const ProgramRequirementDisplay: FC<ProgramRequirementDisplayProps> = ({
  requirement,
  nested,
  takenCourseIDs,
  storeKey,
}) => {
  switch (requirement.requirementType) {
    case 'Unit':
    case 'Course': {
      return nested ? (
        <GroupedCourseRequirement data={requirement} takenCourseIDs={takenCourseIDs} />
      ) : (
        <CourseRequirement data={requirement} storeKey={storeKey} takenCourseIDs={takenCourseIDs} />
      );
    }
    case 'Group':
      return <GroupRequirement data={requirement} storeKey={storeKey} takenCourseIDs={takenCourseIDs} />;
  }
};

interface RequireCourseListProps {
  requirements: ProgramRequirement[];
  storeKeyPrefix: string;
}

const ProgramRequirementsList: FC<RequireCourseListProps> = ({ requirements, storeKeyPrefix }) => {
  const collapsedRequirements = flattenSingletonGroups(collapseSingletonRequirements(requirements));
  const transferredCourses = useTransferredCredits().courses;
  const roadmapPlans = useAppSelector((state) => state.roadmap.plans);
  const roadmapPlanIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const yearPlans = roadmapPlans[roadmapPlanIndex].content.yearPlans;

  const roadmapCourseMap = yearPlans
    .flatMap((year) => year.quarters)
    .flatMap((quarter) => quarter.courses)
    .map((course) => [course.id, course.minUnits]);
  const transferCourseMap = transferredCourses.map((t) => [t.courseName.replace(/\s/g, ''), t.units ?? 0]);

  const takenCourseSet: CompletedCourseSet = Object.assign(
    {},
    Object.fromEntries(roadmapCourseMap),
    Object.fromEntries(transferCourseMap),
  );

  return (
    <div className="program-requirements">
      {/* key is ok because we don't reorder these */}
      {collapsedRequirements.map((r, i) => (
        <ProgramRequirementDisplay
          requirement={r}
          key={i}
          storeKey={`${storeKeyPrefix}-${i}`}
          takenCourseIDs={takenCourseSet}
        />
      ))}
    </div>
  );
};

export default ProgramRequirementsList;
