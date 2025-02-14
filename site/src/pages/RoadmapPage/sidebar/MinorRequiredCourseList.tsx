import { FC, useCallback, useContext, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import Select from 'react-select';
import trpc from '../../../trpc';
import { normalizeMajorName, comboboxTheme } from '../../../helpers/courseRequirements';
import { Spinner } from 'react-bootstrap';
import { setRequirements, setMinor, setMinorList } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import ThemeContext from '../../../style/theme-context';
import { MinorProgram } from '@peterportal/types';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';

function getCoursesForMinor(programId: string) {
  return trpc.programs.getRequiredCourses.query({ type: 'minor', programId });
}

const MinorRequiredCourseList: FC = () => {
  const isDark = useContext(ThemeContext).darkMode;
  const isLoggedIn = useIsLoggedIn();
  const minors = useAppSelector((state) => state.courseRequirements.minorList);
  const selectedMinor = useAppSelector((state) => state.courseRequirements.minor);

  const plans = useAppSelector((state) => state.roadmap.plans);
  const planIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const activePlanID = plans[planIndex].id;

  /** Must ONLY contain requirements for the selected major/spec. This is used to check whether a major is
   * already loaded, so it must be set to empty if we change the major or spec */
  const requirements = useAppSelector((state) => state.courseRequirements.currentRequirements);

  const [minorsLoading, setMinorsLoading] = useState(false);
  const [resultsLoading, setResultsLoading] = useState(false);

  const dispatch = useAppDispatch();

  // Initial Load Helpers
  const saveInitialMinorList = useCallback(
    (minors: MinorProgram[]) => {
      for (const m of minors) m.name = normalizeMajorName(m);
      minors.sort((a, b) => a.name.localeCompare(b.name));
      setMinorsLoading(false);
      dispatch(setMinorList(minors));
    },
    [dispatch],
  );

  // Initial Load, fetch minors
  useEffect(() => {
    if (minors.length) return;
    trpc.programs.getMinors.query().then(saveInitialMinorList);
  }, [dispatch, minors.length, saveInitialMinorList]);

  // Fetch requirements for minor
  useEffect(() => {
    if (!selectedMinor?.id) return;

    setResultsLoading(true);
    getCoursesForMinor(selectedMinor.id).then(async (minorReqs) => {
      dispatch(setRequirements(minorReqs));
      setResultsLoading(false);
      return;
    });
  }, [dispatch, selectedMinor]);

  // Switching roadmaps should restore minor
  useEffect(() => {
    if (!minors.length || !activePlanID) return;
    if (!activePlanID) return;
    if (!isLoggedIn) return;

    setMinorsLoading(true);
    trpc.programs.getMinors.query();
    setMinorsLoading(false);

    const foundMinor = minors.find((m) => m.id === selectedMinor?.id) ?? null;
    dispatch(setMinor(foundMinor));
    setResultsLoading(true);
    if (!foundMinor) return;
    getCoursesForMinor(foundMinor.id).then(async (minorReqs) => {
      dispatch(setRequirements(minorReqs));
      setResultsLoading(false);
      return;
    });
  }, [dispatch, activePlanID, minors, isLoggedIn, selectedMinor?.id]);

  const minorSelectOptions = minors.map((m) => ({
    value: m,
    label: `${m.name}`,
  }));

  const loadingIcon = (
    <div className="requirements-loading">
      <Spinner animation="border" />
    </div>
  );

  return (
    <>
      <Select
        options={minorSelectOptions}
        // must be NULL, not undefined, to clear the field for unselected values
        value={minorSelectOptions.find((o) => o.value === selectedMinor) ?? null}
        isDisabled={minorsLoading}
        isLoading={minorsLoading}
        onChange={(data) => {
          if (data!.value.id === selectedMinor?.id) return;
          dispatch(setRequirements([])); // set to empty immediately because otherwise it's out of date
          dispatch(setMinor(data!.value));
        }}
        className="ppc-combobox"
        classNamePrefix="ppc-combobox"
        placeholder="Select a minor..."
        theme={(t) => comboboxTheme(t, isDark)}
      />
      {selectedMinor && <>{resultsLoading ? loadingIcon : <ProgramRequirementsList requirements={requirements} />}</>}
    </>
  );
};

export default MinorRequiredCourseList;
