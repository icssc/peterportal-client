import { FC, useCallback, useEffect, useState, useRef } from 'react';
import { Autocomplete, TextField } from '@mui/material';
import trpc from '../../../trpc';
import { normalizeMajorName } from '../../../helpers/courseRequirements';
import { addMinor, removeMinor, setMinorList, MinorRequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { MinorProgram } from '@peterportal/types';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import MinorCourseList from './MinorCourseList';

function updateSelectedMinors(plannerId: number, minorIds: string[]) {
  if (!plannerId) return;
  trpc.programs.saveSelectedMinor.mutate({ plannerId, minorIds });
}

interface MinorOption {
  value: MinorProgram;
  label: string;
}

const MinorSelector: FC = () => {
  const isLoggedIn = useIsLoggedIn();
  const minors = useAppSelector((state) => state.courseRequirements.minorList);
  const selectedMinors = useAppSelector((state) => state.courseRequirements.selectedMinors);

  const plans = useAppSelector((state) => state.roadmap.plans);
  const planIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const activePlanID = plans[planIndex].id;

  const [minorsLoading, setMinorsLoading] = useState(false);

  const dispatch = useAppDispatch();

  const planEffectRef = useRef<number | null>(null);

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

  const saveMinors = useCallback(
    (minorsToSave: MinorRequirements[]) => {
      if (!activePlanID || !isLoggedIn) return;
      const minorIds: string[] = minorsToSave.map((m) => m.minor.id);
      updateSelectedMinors(activePlanID, minorIds);
    },
    [activePlanID, isLoggedIn],
  );

  const handleMinorChange = useCallback(
    (_event: unknown, selections: MinorOption[] | null) => {
      const newMinors = selections?.map((s) => s.value) || [];
      const currentMinorIds = selectedMinors.map((m) => m.minor.id);

      currentMinorIds.forEach((id) => {
        if (!newMinors.find((m) => m.id === id)) dispatch(removeMinor(id));
      });
      newMinors.forEach((minor) => {
        if (!currentMinorIds.includes(minor.id)) dispatch(addMinor(minor));
      });

      const updatedMinors = newMinors.map((minor) => ({
        minor,
        requirements: selectedMinors.find((m) => m.minor.id === minor.id)?.requirements || [],
      }));
      saveMinors(updatedMinors);
    },
    [dispatch, saveMinors, selectedMinors],
  );

  useEffect(() => {
    if (!minors.length || !activePlanID || !isLoggedIn) return;
    if (planEffectRef.current === activePlanID) return;
    if (selectedMinors.length) return;

    setMinorsLoading(true);
    trpc.programs.getSavedMinors
      .query(activePlanID)
      .then((minorIds) => {
        planEffectRef.current = activePlanID;
        for (const minor of minorIds) {
          const foundMinor = minors.find((m) => m.id === minor.id);
          if (!foundMinor) continue;
          dispatch(addMinor(foundMinor));
        }
      })
      .finally(() => {
        setMinorsLoading(false);
      });
  }, [dispatch, activePlanID, minors, isLoggedIn, selectedMinors.length]);

  const minorSelectOptions: MinorOption[] = minors.map((m) => ({
    value: m,
    label: `${m.name}`,
  }));

  return (
    <>
      <Autocomplete
        multiple
        options={minorSelectOptions}
        value={selectedMinors.map((m) => minorSelectOptions.find((o) => o.value.id === m.minor.id)!).filter(Boolean)}
        onChange={handleMinorChange}
        getOptionLabel={(option) => option.label}
        isOptionEqualToValue={(option, value) => option.value.id === value.value.id}
        loading={minorsLoading}
        disabled={minorsLoading}
        className="minor-select"
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
          <TextField {...params} variant="outlined" size="small" placeholder="Select minors..." />
        )}
      />
      {selectedMinors.map((data) => (
        <MinorCourseList key={data.minor.id} minorReqs={data} />
      ))}
    </>
  );
};

export default MinorSelector;
