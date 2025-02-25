import { FC, useCallback, useContext, useEffect, useState, useRef } from 'react';
import Select from 'react-select';
import trpc from '../../../trpc';
import { normalizeMajorName, comboboxTheme } from '../../../helpers/courseRequirements';
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
import SingleMajorCourseList from './SingleMajorCourseList';

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
  const [expandedMajors, setExpandedMajors] = useState<{ [majorId: string]: boolean }>({});

  const plans = useAppSelector((state) => state.roadmap.plans);
  const planIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const activePlanID = plans[planIndex].id;
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

  const loadingMajorsRef = useRef<Set<string>>(new Set());
  const planEffectRef = useRef<number | null>(null);

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

  const fetchSpecializations = useCallback(async (majorId: string) => {
    if (loadingMajorsRef.current.has(majorId)) {
      return;
    }
    loadingMajorsRef.current.add(majorId);
    setSpecsLoading((prev) => ({ ...prev, [majorId]: true }));

    try {
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
    } finally {
      loadingMajorsRef.current.delete(majorId);
      setSpecsLoading((prev) => ({ ...prev, [majorId]: false }));
    }
  }, []);

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
        if (majorId) {
          await fetchSpecializations(majorId);
        } else {
          setSpecOptions((prev) => ({ ...prev, [majorId]: [] }));
        }
      } finally {
        setResultsLoading((prev) => ({ ...prev, [majorId]: false }));
      }
    },
    [dispatch, fetchSpecializations],
  );

  const handleMajorChange = useCallback(
    (
      selections:
        | readonly {
            value: MajorProgram;
            label: string;
          }[]
        | null,
    ) => {
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
          setExpandedMajors((prev) => ({
            ...prev,
            [major.id]: true,
          }));
        }
      });

      const updatedMajors = newMajors.map((major) => ({
        major,
        specialization: selectedMajors.find((m) => m.major.id === major.id)?.specialization || null,
        requirements: selectedMajors.find((m) => m.major.id === major.id)?.requirements || [],
      }));
      saveMajors(updatedMajors);
    },
    [dispatch, fetchRequirements, saveMajors, selectedMajors],
  );

  const handleSpecializationChange = useCallback(
    async (majorId: string, data: { value: MajorSpecialization; label: string } | null) => {
      const updatedSpec = data?.value || null;
      setResultsLoading((prev) => ({ ...prev, [majorId]: true }));
      dispatch(setRequirements({ majorId, requirements: [] }));
      dispatch(
        setSpecialization({
          majorId,
          specialization: updatedSpec,
        }),
      );
      await fetchRequirements(majorId, updatedSpec?.id || null);
      const updatedMajors = selectedMajors.map((m) =>
        m.major.id === majorId ? { ...m, specialization: updatedSpec } : m,
      );
      saveMajors(updatedMajors);
    },
    [dispatch, fetchRequirements, saveMajors, selectedMajors],
  );

  useEffect(() => {
    if (!majors.length || !activePlanID || !isLoggedIn) return;
    if (planEffectRef.current === activePlanID) {
      return;
    }

    setMajorsLoading(true);
    const loadMajorsAndSpecs = async () => {
      try {
        const pairs = await trpc.programs.getSavedMajorSpecPairs.query(activePlanID);

        planEffectRef.current = activePlanID;

        const currentMajorIds = selectedMajors.map((m) => m.major.id);
        currentMajorIds.forEach((id) => dispatch(removeMajor(id)));

        for (const pair of pairs) {
          const foundMajor = majors.find((m) => m.id === pair.majorId);
          if (!foundMajor) continue;
          dispatch(addMajor(foundMajor));

          if (pair.specializationId && foundMajor.specializations.length) {
            await fetchSpecializations(foundMajor.id);
            const specs = await getMajorSpecializations(foundMajor.id);
            const foundSpec = specs.find((s) => s.id === pair.specializationId);
            if (foundSpec) {
              dispatch(setSpecialization({ majorId: foundMajor.id, specialization: foundSpec }));
              await fetchRequirements(foundMajor.id, pair.specializationId);
            }
          } else if (foundMajor.specializations.length === 0) {
            await fetchRequirements(foundMajor.id, null);
          }
        }
      } finally {
        setMajorsLoading(false);
      }
    };

    loadMajorsAndSpecs();
  }, [dispatch, activePlanID, majors, isLoggedIn, fetchRequirements, fetchSpecializations, selectedMajors]);

  useEffect(() => {
    if (selectedMajors.length > 0) {
      const expandedState = selectedMajors.reduce(
        (acc, major) => ({
          ...acc,
          [major.major.id]: true,
        }),
        {},
      );
      setExpandedMajors(expandedState);
    }
  }, [selectedMajors]);

  const majorSelectOptions = majors.map((m) => ({
    value: m,
    label: `${m.name}, ${m.type}`,
  }));

  const handleToggleExpand = useCallback((majorId: string) => {
    setExpandedMajors((prev) => ({
      ...prev,
      [majorId]: !prev[majorId],
    }));
  }, []);

  return (
    <>
      <Select
        isMulti
        options={majorSelectOptions}
        value={selectedMajors.map((m) => majorSelectOptions.find((o) => o.value.id === m.major.id)!)}
        isDisabled={majorsLoading}
        isLoading={majorsLoading}
        onChange={handleMajorChange}
        className="ppc-combobox"
        classNamePrefix="ppc-combobox"
        placeholder="Select majors..."
        theme={(t) => comboboxTheme(t, isDark)}
      />

      {selectedMajors.map((majorWithSpec) => (
        <SingleMajorCourseList
          key={majorWithSpec.major.id}
          majorWithSpec={majorWithSpec}
          isExpanded={!!expandedMajors[majorWithSpec.major.id]}
          onToggleExpand={handleToggleExpand}
          specOptions={specOptions[majorWithSpec.major.id] || []}
          specsLoading={specsLoading[majorWithSpec.major.id]}
          resultsLoading={resultsLoading[majorWithSpec.major.id]}
          onSpecializationChange={handleSpecializationChange}
          fetchRequirements={fetchRequirements}
        />
      ))}
    </>
  );
};

export default MajorRequiredCourseList;
