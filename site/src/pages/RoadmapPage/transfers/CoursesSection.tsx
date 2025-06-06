import { FC, useState, useContext } from 'react';
import AsyncSelect from 'react-select/async';

import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';

import trpc from '../../../trpc';
import { TransferredCourse, CourseAAPIResponse } from '@peterportal/types';
import ThemeContext from '../../../style/theme-context';
import {
  addTransferredCourse,
  removeTransferredCourse,
  updateTransferredCourse,
} from '../../../store/slices/transferCreditsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { comboboxTheme } from '../../../helpers/courseRequirements';
import { getCourseIdWithSpaces } from '../../../helpers/util';

interface CourseSelectOption {
  value: TransferredCourse;
  label: string;
}

interface CourseCreditMenuTileProps {
  course: TransferredCourse;
}
const CourseCreditMenuTile: FC<CourseCreditMenuTileProps> = ({ course }) => {
  const dispatch = useAppDispatch();

  const deleteFn = () => {
    trpc.transferCredits.removeTransferredCourse.mutate(course.courseName);
    dispatch(removeTransferredCourse(course.courseName));
  };
  const setUnits = (value: number) => {
    const updatedCourse: TransferredCourse = { courseName: course.courseName, units: value };
    trpc.transferCredits.updateTransferredCourse.mutate(updatedCourse);
    dispatch(updateTransferredCourse(updatedCourse));
  };

  return <MenuTile title={course.courseName} units={course.units} setUnits={setUnits} deleteFn={deleteFn} />;
};

const CoursesSection: FC = () => {
  const courses = useAppSelector((state) => state.transferCredits.transferredCourses);
  const [timeout, setTimeout] = useState<number | null>(null);
  const [abortFn, setAbortFn] = useState<() => void>();
  const dispatch = useAppDispatch();
  const isDark = useContext(ThemeContext).darkMode;

  const cancelIncompleteSearch = () => {
    abortFn?.();
    if (timeout) clearTimeout(timeout);
  };

  const loadCourses = async (query: string) => {
    const response = await trpc.search.get.query({ query, skip: 0, take: 10, resultType: 'course' });
    const courses = response.results.map((c) => c.result) as CourseAAPIResponse[];
    const options: CourseSelectOption[] = courses.map((c) => ({
      value: { courseName: getCourseIdWithSpaces(c), units: c.maxUnits },
      label: `${getCourseIdWithSpaces(c)}: ${c.title}`,
    }));
    return options;
  };

  const loadAfterTimeout = async (query: string) => {
    if (timeout) clearTimeout(timeout);
    return new Promise<CourseSelectOption[]>((resolve) => {
      const initSearch = async () => {
        const abortPromise = new Promise<null>((resolve) => setAbortFn(() => resolve));
        const response = await Promise.race([abortPromise, loadCourses(query)]);
        if (response) resolve(response);
      };
      setTimeout(window.setTimeout(initSearch, 300));
    });
  };

  const addCourse = (course: TransferredCourse) => {
    dispatch(addTransferredCourse(course));
    trpc.transferCredits.addTransferredCourse.mutate(course);
  };

  return (
    <MenuSection title="Courses You've Transferred">
      <SectionDescription>
        Enter courses you&rsquo;ve claimed credit for through a{' '}
        <a href="https://testingcenter.uci.edu/" target="_blank" rel="noreferrer">
          credit by exam
        </a>{' '}
        or{' '}
        <a href="https://assist.org" target="_blank" rel="noreferrer">
          through another college
        </a>
        .
      </SectionDescription>

      {courses.map((course) => (
        <CourseCreditMenuTile key={course.courseName} course={course} />
      ))}

      <AsyncSelect
        className="ppc-combobox"
        classNamePrefix="ppc-combobox"
        placeholder="Search for a course to add..."
        theme={(t) => comboboxTheme(t, isDark)}
        cacheOptions
        onInputChange={cancelIncompleteSearch}
        loadOptions={loadAfterTimeout}
        onChange={(option) => addCourse(option!.value)}
        noOptionsMessage={({ inputValue }) => (inputValue ? 'No courses found' : null)}
        value={null}
        components={{ DropdownIndicator: () => null, IndicatorSeparator: () => null }}
      />
    </MenuSection>
  );
};

export default CoursesSection;
