import { FC, useCallback, useContext, useEffect, useState } from 'react';
import Select from 'react-select';
import trpc from '../../../trpc';
import { normalizeMajorName, comboboxTheme } from '../../../helpers/courseRequirements';
import { addMinor, removeMinor, setMinorList, minorRequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import ThemeContext from '../../../style/theme-context';
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
  const isDark = useContext(ThemeContext).darkMode;
  const isLoggedIn = useIsLoggedIn();
  const minors = useAppSelector((state) => state.courseRequirements.minorList);
  const selectedMinors = useAppSelector((state) => state.courseRequirements.selectedMinors);

  const plans = useAppSelector((state) => state.roadmap.plans);
  const planIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const activePlanID = plans[planIndex].id;

  const [minorsLoading, setMinorsLoading] = useState(false);
  // const [resultsLoading, setResultsLoading] = useState(false);

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

  const saveMinors = useCallback(
    (minorsToSave: minorRequirements[]) => {
      if (!activePlanID || !isLoggedIn) return;
      const minorIds: string[] = minorsToSave.map((m) => m.minor.id);
      updateSelectedMinors(activePlanID, minorIds);
    },
    [activePlanID, isLoggedIn],
  );

  const handleMinorChange = useCallback(
    (selections: readonly MinorOption[] | null) => {
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
    if (!minors.length || !activePlanID) return;
    if (!activePlanID) return;
    if (!isLoggedIn) return;

    setMinorsLoading(true);
    trpc.programs.getSavedMinors
      .query(activePlanID)
      .then((minorIds) => {
        for (const minor of minorIds) {
          const foundMinor = minors.find((m) => m.id === minor.id);
          if (!foundMinor) continue;
          dispatch(addMinor(foundMinor));
        }
      })
      .finally(() => {
        setMinorsLoading(false);
      });
  }, [dispatch, activePlanID, minors, isLoggedIn, selectedMinors]);

  const minorSelectOptions: MinorOption[] = minors.map((m) => ({
    value: m,
    label: `${m.name}`,
  }));

  return (
    <>
      <Select
        isMulti
        options={minorSelectOptions}
        value={selectedMinors.map((m) => minorSelectOptions.find((o) => o.value.id === m.minor.id)!)}
        isDisabled={minorsLoading}
        isLoading={minorsLoading}
        onChange={handleMinorChange}
        className="ppc-combobox"
        classNamePrefix="ppc-combobox"
        placeholder="Select minors..."
        theme={(t) => comboboxTheme(t, isDark)}
      />
      {selectedMinors.map((data) => (
        <MinorCourseList key={data.minor.id} minorReqs={data} />
      ))}
    </>
  );
};

export default MinorSelector;
