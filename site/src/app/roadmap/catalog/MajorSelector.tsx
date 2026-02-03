import { FC, useCallback, useEffect, useState, useRef } from 'react';
import { Autocomplete, TextField } from '@mui/material';
import trpc from '../../../trpc';
import { normalizeMajorName } from '../../../helpers/courseRequirements';
import {
  addMajor,
  removeMajor,
  setMajorList,
  MajorWithSpecialization,
} from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { MajorProgram, MajorSpecialization, MajorSpecializationPair } from '@peterportal/types';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import MajorCourseList from './MajorCourseList';

function updateSelectedMajorAndSpecialization(plannerId: number, pairs: MajorSpecializationPair[]) {
  if (!plannerId) return;
  trpc.programs.saveSelectedMajorSpecPair.mutate({ plannerId, pairs });
}

interface MajorOption {
  value: MajorProgram;
  label: string;
}

const MajorSelector: FC = () => {
  const isLoggedIn = useIsLoggedIn();
  const majors = useAppSelector((state) => state.courseRequirements.majorList);
  const selectedMajors = useAppSelector((state) => state.courseRequirements.selectedMajors);
  const [defaultPairs, setDefaultPairs] = useState<MajorSpecializationPair[]>([]);

  const plans = useAppSelector((state) => state.roadmap.plans);
  const planIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const activePlanID = plans[planIndex].id;
  const [majorsLoading, setMajorsLoading] = useState(false);

  const dispatch = useAppDispatch();

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
        specializationId: m.selectedSpec?.id,
      }));
      updateSelectedMajorAndSpecialization(activePlanID, pairs);
    },
    [activePlanID, isLoggedIn],
  );

  const handleMajorChange = useCallback(
    (_event: unknown, selections: MajorOption[] | null) => {
      const newMajors = selections?.map((s) => s.value) || [];
      const currentMajorIds = selectedMajors.map((m) => m.major.id);

      currentMajorIds.forEach((id) => {
        if (!newMajors.find((m) => m.id === id)) dispatch(removeMajor(id));
      });
      newMajors.forEach((major) => {
        if (!currentMajorIds.includes(major.id)) dispatch(addMajor(major));
      });

      const updatedMajors = newMajors.map((major) => ({
        major,
        selectedSpec: selectedMajors.find((m) => m.major.id === major.id)?.selectedSpec || null,
        specializations: selectedMajors.find((m) => m.major.id === major.id)?.specializations || [],
        requirements: selectedMajors.find((m) => m.major.id === major.id)?.requirements || [],
      }));
      saveMajors(updatedMajors);
    },
    [dispatch, saveMajors, selectedMajors],
  );

  const handleSpecializationChange = useCallback(
    async (majorId: string, specialization: MajorSpecialization | null) => {
      const updatedMajors = selectedMajors.map((m) =>
        m.major.id === majorId ? { ...m, selectedSpec: specialization } : m,
      );
      setDefaultPairs(
        defaultPairs.map((p) => (p.majorId === majorId ? { ...p, specializationId: specialization?.id } : p)),
      );
      saveMajors(updatedMajors);
    },
    [defaultPairs, saveMajors, selectedMajors],
  );

  useEffect(() => {
    if (!majors.length || !activePlanID || !isLoggedIn) return;
    if (planEffectRef.current === activePlanID) return;
    if (selectedMajors.length) return;

    setMajorsLoading(true);

    trpc.programs.getSavedMajorSpecPairs
      .query(activePlanID)
      .then((pairs) => {
        planEffectRef.current = activePlanID;
        const currentMajorIds = selectedMajors.map((m) => m.major.id);
        currentMajorIds.forEach((id) => dispatch(removeMajor(id)));

        for (const pair of pairs) {
          const foundMajor = majors.find((m) => m.id === pair.majorId);
          if (!foundMajor) continue;
          dispatch(addMajor(foundMajor));
        }
        setDefaultPairs(pairs);
      })
      .finally(() => {
        setMajorsLoading(false);
      });
  }, [dispatch, activePlanID, majors, isLoggedIn, selectedMajors]);

  const majorSelectOptions: MajorOption[] = majors.map((m) => ({
    value: m,
    label: `${m.name}, ${m.type}`,
  }));

  return (
    <>
      <Autocomplete
        multiple
        options={majorSelectOptions}
        value={selectedMajors.map((m) => majorSelectOptions.find((o) => o.value.id === m.major.id)!).filter(Boolean)}
        onChange={handleMajorChange}
        getOptionLabel={(option) => option.label}
        isOptionEqualToValue={(option, value) => option.value.id === value.value.id}
        loading={majorsLoading}
        disabled={majorsLoading}
        className="major-select"
        slotProps={{
          listbox: {
            sx: {
              padding: 0,
            },
          },
          paper: {
            sx: {
              marginTop: 0,
              borderRadius: '0 0 8px 8px',
              overflow: 'hidden',
            },
          },
        }}
        renderInput={(params) => (
          <TextField {...params} variant="outlined" size="small" placeholder="Select majors..." />
        )}
      />
      {selectedMajors.map((data) => (
        <MajorCourseList
          key={data.major.id}
          majorWithSpec={data}
          selectedSpecId={defaultPairs.find((p) => p.majorId === data.major.id)?.specializationId}
          onSpecializationChange={handleSpecializationChange}
        />
      ))}
    </>
  );
};

export default MajorSelector;
