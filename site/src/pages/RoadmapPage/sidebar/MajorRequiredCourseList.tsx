import { FC, useCallback, useContext, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import Select from 'react-select';
import trpc from '../../../trpc';
import { normalizeMajorName, comboboxTheme } from '../../../helpers/courseRequirements';
import { Spinner } from 'react-bootstrap';
import {
  addMajor,
  removeMajor,
  setMajorList,
  setRequirements,
  setSpecialization,
  MajorWithSpecialization,
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
  const selectedMajors = useAppSelector((state) => state.courseRequirements.selectedMajors);

  const plans = useAppSelector((state) => state.roadmap.plans);
  const planIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const activePlanID = plans[planIndex].id;

  /** Must ONLY contain requirements for the selected major/spec. This is used to check whether a major is
   * already loaded, so it must be set to empty if we change the major or spec */

  const [majorsLoading, setMajorsLoading] = useState(false);
  const [specsLoading, setSpecsLoading] = useState<{ [majorId: string]: boolean }>({});
  const [resultsLoading, setResultsLoading] = useState<{ [majorId: string]: boolean }>({});
  const [specOptions, setSpecOptions] = useState<{
    [majorId: string]: {
      value: MajorSpecialization;
      label: string;
    }[];
  }>({});

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

  const saveMajors = useCallback(
    (majorsToSave: MajorWithSpecialization[]) => {
      if (!activePlanID || !isLoggedIn) return;
      const pairs: MajorSpecializationPair[] = majorsToSave.map((m) => ({
        majorId: m.major.id,
        specializationId: m.specialization?.id,
      }));
      updateSelectedMajorAndSpecialization(activePlanID, pairs);
    },
    [activePlanID, isLoggedIn],
  );

  const fetchRequirements = useCallback(
    async (majorId: string, specializationId?: string | null) => {
      setResultsLoading((prev) => ({ ...prev, [majorId]: true }));

      try {
        const majorReqs = await getCoursesForMajor(majorId);
        let allReqs = majorReqs;

        if (specializationId) {
          const specReqs = await getCoursesForSpecialization(specializationId);
          allReqs = majorReqs.concat(specReqs);
        }

        dispatch(setRequirements({ majorId, requirements: allReqs }));
        if (!specsLoading[majorId] && !specOptions[majorId]) {
          // console.log('fetching specs');
          setSpecsLoading((prev) => ({ ...prev, [majorId]: true }));
          const specs = await getMajorSpecializations(majorId);
          specs.forEach((s) => (s.name = normalizeMajorName(s)));
          specs.sort((a, b) => a.name.localeCompare(b.name));
          const options = specs.map((s) => ({
            value: s,
            label: s.name,
          }));
          setSpecOptions((prev) => ({
            ...prev,
            [majorId]: options,
          }));
          setSpecsLoading((prev) => ({ ...prev, [majorId]: false }));
        }
      } finally {
        setResultsLoading((prev) => ({ ...prev, [majorId]: false }));
      }
    },
    [dispatch],
  );

  // Switching roadmaps should restore major/spec
  useEffect(() => {
    if (!majors.length || !activePlanID || !isLoggedIn) return;

    let mounted = true;
    setMajorsLoading(true);

    const loadMajorsAndSpecs = async () => {
      try {
        const pairs = await trpc.programs.getSavedMajorSpecPairs.query(activePlanID);
        if (!mounted) return; //mounted is false if the component is unmounted, prevents loop rendering issue i had
        const currentMajorIds = selectedMajors.map((m) => m.major.id);
        currentMajorIds.forEach((id) => dispatch(removeMajor(id)));
        for (const pair of pairs) {
          const foundMajor = majors.find((m) => m.id === pair.majorId);
          if (!foundMajor || !mounted) continue;

          dispatch(addMajor(foundMajor));

          if (pair.specializationId && foundMajor.specializations.length) {
            setSpecsLoading((prev) => ({ ...prev, [foundMajor.id]: true }));
            const specs = await getMajorSpecializations(foundMajor.id);
            if (!mounted) return;
            const foundSpec = specs.find((s) => s.id === pair.specializationId);
            if (foundSpec) {
              dispatch(setSpecialization({ majorId: foundMajor.id, specialization: foundSpec }));
            }
            setSpecsLoading((prev) => ({ ...prev, [foundMajor.id]: false }));
          }

          await fetchRequirements(foundMajor.id, pair.specializationId);
        }
      } finally {
        if (mounted) {
          setMajorsLoading(false);
        }
      }
    };

    loadMajorsAndSpecs();

    return () => {
      mounted = false;
    };
  }, [dispatch, activePlanID, majors, isLoggedIn, fetchRequirements]);

  const majorSelectOptions = majors.map((m) => ({
    value: m,
    label: `${m.name}, ${m.type}`,
  }));

  const loadingIcon = (
    <div className="requirements-loading">
      <Spinner animation="border" />
    </div>
  );

  return (
    <>
      <Select
        isMulti
        options={majorSelectOptions}
        value={selectedMajors.map((m) => majorSelectOptions.find((o) => o.value.id === m.major.id)!)}
        isDisabled={majorsLoading}
        isLoading={majorsLoading}
        onChange={(selections) => {
          const newMajors = selections?.map((s) => s.value) || [];
          const currentMajorIds = selectedMajors.map((m) => m.major.id);
          currentMajorIds.forEach((id) => {
            if (!newMajors.find((m) => m.id === id)) {
              dispatch(removeMajor(id));
            }
          });
          newMajors.forEach((major) => {
            if (!currentMajorIds.includes(major.id)) {
              dispatch(addMajor(major));
              fetchRequirements(major.id);
            }
          });
          const updatedMajors = newMajors.map((major) => ({
            major,
            specialization: selectedMajors.find((m) => m.major.id === major.id)?.specialization || null,
            requirements: selectedMajors.find((m) => m.major.id === major.id)?.requirements || [],
          }));
          saveMajors(updatedMajors);
        }}
        className="ppc-combobox"
        classNamePrefix="ppc-combobox"
        placeholder="Select majors..."
        theme={(t) => comboboxTheme(t, isDark)}
      />

      {selectedMajors.map((majorWithSpec) => (
        <div key={majorWithSpec.major.id} className="major-section mt-3">
          <h4>{majorWithSpec.major.name}</h4>
          {majorWithSpec.major.specializations.length > 0 && (
            <Select
              options={specOptions[majorWithSpec.major.id] || []}
              value={
                majorWithSpec.specialization
                  ? {
                      value: majorWithSpec.specialization,
                      label: majorWithSpec.specialization.name,
                    }
                  : null
              }
              isDisabled={specsLoading[majorWithSpec.major.id]}
              isLoading={specsLoading[majorWithSpec.major.id]}
              onChange={async (data) => {
                const updatedSpec = data?.value || null;
                setResultsLoading((prev) => ({ ...prev, [majorWithSpec.major.id]: true }));
                dispatch(setRequirements({ majorId: majorWithSpec.major.id, requirements: [] })); // set to empty immediately because otherwise it's out of date
                dispatch(
                  setSpecialization({
                    majorId: majorWithSpec.major.id,
                    specialization: updatedSpec,
                  }),
                );
                await fetchRequirements(majorWithSpec.major.id, updatedSpec?.id || null);
                const updatedMajors = selectedMajors.map((m) =>
                  m.major.id === majorWithSpec.major.id ? { ...m, specialization: updatedSpec } : m,
                );
                saveMajors(updatedMajors);
              }}
              className="ppc-combobox"
              classNamePrefix="ppc-combobox"
              placeholder="Select a specialization..."
              theme={(t) => comboboxTheme(t, isDark)}
              onFocus={async () => {}}
            />
          )}
          {resultsLoading[majorWithSpec.major.id] ? (
            loadingIcon
          ) : (
            <ProgramRequirementsList requirements={majorWithSpec.requirements} />
          )}
        </div>
      ))}
    </>
  );
};

export default MajorRequiredCourseList;
