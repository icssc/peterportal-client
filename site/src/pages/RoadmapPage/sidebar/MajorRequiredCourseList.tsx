import { FC, useCallback, useContext, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import Select from 'react-select';
import trpc from '../../../trpc';
import { normalizeMajorName, comboboxTheme } from '../../../helpers/courseRequirements';
import { Spinner } from 'react-bootstrap';
import {
  setMajor,
  setMajorList,
  setRequirements,
  setSpecialization,
  setSpecializationList,
} from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import ThemeContext from '../../../style/theme-context';
import { MajorProgram, MajorSpecialization, MajorSpecializationPair } from '@peterportal/types';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';

function updateSelectedMajorAndSpecialization(plannerId: number, pairs: MajorSpecializationPair[]) {
  if (!plannerId) return;
  trpc.programs.saveSelectedMajorSpecPair.mutate({ plannerId, pairs });
}

function getMajorSpecializations(majorId: string) {
  return trpc.programs.getSpecializations.query({ major: majorId });
}

function getCoursesForMajor(programId: string) {
  return trpc.programs.getRequiredCourses.query({ type: 'major', programId });
}

function getCoursesForSpecialization(programId: string) {
  return trpc.programs.getRequiredCourses.query({ type: 'specialization', programId });
}

const MajorRequiredCourseList: FC = () => {
  const isDark = useContext(ThemeContext).darkMode;
  const isLoggedIn = useIsLoggedIn();
  const majors = useAppSelector((state) => state.courseRequirements.majorList);
  const specializations = useAppSelector((state) => state.courseRequirements.specializationList);
  const selectedMajor = useAppSelector((state) => state.courseRequirements.major);
  const selectedSpec = useAppSelector((state) => state.courseRequirements.specialization);

  const plans = useAppSelector((state) => state.roadmap.plans);
  const planIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const activePlanID = plans[planIndex].id;

  /** Must ONLY contain requirements for the selected major/spec. This is used to check whether a major is
   * already loaded, so it must be set to empty if we change the major or spec */
  const requirements = useAppSelector((state) => state.courseRequirements.currentRequirements);

  const [savedSpecID, setSavedSpecID] = useState<string | null>(null);

  const [majorsLoading, setMajorsLoading] = useState(false);
  const [specsLoading, setSpecsLoading] = useState(false);
  const [resultsLoading, setResultsLoading] = useState(false);

  const dispatch = useAppDispatch();

  // Initial Load Helpers
  const saveInitialMajorList = useCallback(
    (majors: MajorProgram[]) => {
      for (const m of majors) m.name = normalizeMajorName(m);
      majors.sort((a, b) => a.name.localeCompare(b.name));
      setMajorsLoading(false);
      dispatch(setMajorList(majors));
    },
    [dispatch],
  );

  // Initial Load, fetch majors
  useEffect(() => {
    if (majors.length) return;
    trpc.programs.getMajors.query().then(saveInitialMajorList);
  }, [dispatch, majors.length, saveInitialMajorList]);

  // Fetch Specs Helpers
  const updateSpecilizationList = useCallback(
    (specs: MajorSpecialization[]) => {
      for (const s of specs) s.name = normalizeMajorName(s);
      specs.sort((a, b) => a.name.localeCompare(b.name));
      dispatch(setSpecializationList(specs));
      setSpecsLoading(false);
    },
    [dispatch],
  );

  const restoreSavedSpecialization = useCallback(
    (specs: MajorSpecialization[]) => {
      if (!selectedMajor) return false;
      const foundSpec = specs?.find((spec) => spec.id === savedSpecID);
      if (foundSpec) dispatch(setSpecialization(foundSpec));
    },
    [dispatch, savedSpecID, selectedMajor],
  );

  const saveMajor = useCallback(
    (major: MajorProgram) => {
      if (!activePlanID || !major?.id || !isLoggedIn) return;
      updateSelectedMajorAndSpecialization(activePlanID, [{ majorId: major.id }]);
    },
    [activePlanID, isLoggedIn],
  );

  // Major with specs selected, fetch specializations
  useEffect(() => {
    if (!selectedMajor?.id) return;
    if (selectedSpec) return;
    if (requirements.length) return;

    dispatch(setSpecializationList([]));
    if (!selectedMajor.specializations.length) return;

    setSpecsLoading(true);
    getMajorSpecializations(selectedMajor.id).then((specs) => {
      updateSpecilizationList(specs);
      restoreSavedSpecialization(specs);
    });
  }, [dispatch, selectedMajor, selectedSpec, requirements.length, updateSpecilizationList, restoreSavedSpecialization]);

  // Final Selection Helpers
  const saveSpecialization = useCallback(
    (specialization: MajorSpecialization) => {
      if (!selectedMajor || !activePlanID || specialization?.id === savedSpecID || !isLoggedIn) return;
      updateSelectedMajorAndSpecialization(activePlanID, [
        { majorId: selectedMajor.id, specializationId: specialization?.id },
      ]);
    },
    [activePlanID, selectedMajor, savedSpecID, isLoggedIn],
  );

  // Spec or Major without specs selected, fetch requirements
  useEffect(() => {
    if (!selectedMajor?.id) return;
    if (selectedMajor.specializations.length && !selectedSpec) return;
    if (requirements.length) return;

    setResultsLoading(true);
    getCoursesForMajor(selectedMajor.id).then(async (majorReqs) => {
      if (!selectedSpec) {
        dispatch(setRequirements(majorReqs));
        setResultsLoading(false);
        return;
      }
      const specReqs = await getCoursesForSpecialization(selectedSpec.id);
      dispatch(setRequirements(majorReqs.concat(specReqs)));
      setResultsLoading(false);
    });
  }, [dispatch, selectedMajor, selectedSpec, requirements]);

  // Switching roadmaps should restore major/spec
  useEffect(() => {
    if (!majors.length || !activePlanID) return;
    if (!activePlanID) return;
    if (!isLoggedIn) return;

    setMajorsLoading(true);
    trpc.programs.getSavedMajorSpecPairs.query(activePlanID).then((pairs) => {
      setMajorsLoading(false);

      const foundMajor = majors.find((m) => m.id === pairs[0]?.majorId) ?? null;
      dispatch(setMajor(foundMajor));
      dispatch(setSpecialization(null));
      setSavedSpecID(pairs[0]?.specializationId);
      dispatch(setRequirements([]));
    });
  }, [dispatch, activePlanID, majors, isLoggedIn]);

  const majorSelectOptions = majors.map((m) => ({
    value: m,
    label: `${m.name}, ${m.type}`,
  }));

  const specSelectOptions = specializations.map((s) => ({ value: s, label: s.name }));

  const loadingIcon = (
    <div className="requirements-loading">
      <Spinner animation="border" />
    </div>
  );

  return (
    <>
      <Select
        options={majorSelectOptions}
        // must be NULL, not undefined, to clear the field for unselected values
        value={majorSelectOptions.find((o) => o.value === selectedMajor) ?? null}
        isDisabled={majorsLoading}
        isLoading={majorsLoading}
        onChange={(data) => {
          if (data!.value.id === selectedMajor?.id) return;
          dispatch(setRequirements([])); // set to empty immediately because otherwise it's out of date
          dispatch(setMajor(data!.value));
          dispatch(setSpecialization(null));
          saveMajor(data!.value);
        }}
        className="ppc-combobox"
        classNamePrefix="ppc-combobox"
        placeholder="Select a major..."
        theme={(t) => comboboxTheme(t, isDark)}
      />
      {selectedMajor && !!selectedMajor.specializations.length && (
        <Select
          options={specSelectOptions}
          value={specSelectOptions.find((o) => o.value === selectedSpec)}
          key={selectedMajor.id} // force re-render on changing major
          isDisabled={specsLoading}
          isLoading={specsLoading}
          onChange={(data) => {
            if (data!.value.id === selectedSpec?.id) return;
            setResultsLoading(true);
            dispatch(setRequirements([])); // set to empty immediately because otherwise it's out of date
            dispatch(setSpecialization(data!.value));
            saveSpecialization(data!.value);
          }}
          className="ppc-combobox"
          classNamePrefix="ppc-combobox"
          placeholder="Select a specialization..."
          theme={(t) => comboboxTheme(t, isDark)}
        />
      )}
      {selectedMajor && (!selectedMajor.specializations.length || selectedSpec) && (
        <>{resultsLoading ? loadingIcon : <ProgramRequirementsList requirements={requirements} />}</>
      )}
    </>
  );
};

export default MajorRequiredCourseList;
