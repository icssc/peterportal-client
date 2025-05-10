import './GESection.scss';
import React, { FC } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import { GEName, GETitle, TransferredGE } from '@peterportal/types';
import trpc from '../../../trpc';
import { setTransferredGE } from '../../../store/slices/transferCreditsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';

const ALL_GE_NAMES: GEName[] = ['GE-1A', 'GE-1B', 'GE-2', 'GE-3', 'GE-4', 'GE-5A', 'GE-5B', 'GE-6', 'GE-7', 'GE-8'];
const GE_TITLE_MAP: Record<GEName, GETitle> = {
  'GE-1A': 'GE Ia: Lower Division Writing',
  'GE-1B': 'GE Ib: Upper Division Writing',
  'GE-2': 'GE II: Science and Technology',
  'GE-3': 'GE III: Social & Behavioral Sciences',
  'GE-4': 'GE IV: Arts and Humanities',
  'GE-5A': 'GE Va: Quantitative Literacy',
  'GE-5B': 'GE Vb: Formal Reasoning',
  'GE-6': 'GE VI: Language Other Than English',
  'GE-7': 'GE VII: Multicultural Studies',
  'GE-8': 'GE VIII: International/Global Issues',
};

interface GEInputProps {
  value: number;
  handleUpdate: (newValue: number) => void;
  valueType: 'numberOfCourses' | 'units';
}

const GEInput: FC<GEInputProps> = ({ value, handleUpdate, valueType }) => {
  const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') (event.target as HTMLInputElement).blur();
  };

  const onBlur = (e: React.FocusEvent<HTMLInputElement>) => {
    if (e.target.value === value.toString()) return;
    if (isNaN(e.target.valueAsNumber) || e.target.valueAsNumber < 0) {
      // Revert change for invalid values
      e.target.value = value.toString();
      return;
    }
    // auto-formats, i.e. removes leading zeros
    e.target.value = e.target.valueAsNumber.toString();
    if (e.target.valueAsNumber === value) return;
    handleUpdate(e.target.valueAsNumber);
  };

  return (
    <input
      className="ge-input"
      type="number"
      min="0"
      step={valueType === 'numberOfCourses' ? '1' : 'any'}
      inputMode={valueType === 'numberOfCourses' ? 'numeric' : 'decimal'}
      defaultValue={value}
      onKeyDown={handleKeyDown}
      onBlur={onBlur}
    />
  );
};

interface GEMenuTileProps {
  geName: GEName;
}

const GEMenuTile: FC<GEMenuTileProps> = ({ geName }) => {
  const dispatch = useAppDispatch();

  const currentGE = useAppSelector((state) =>
    state.transferCredits.transferredGEs.find((ge) => ge.geName === geName),
  ) || { geName, numberOfCourses: 0, units: 0 };

  const updateGE = (newCourseCount: number, newUnitCount: number) => {
    const updatedGE: TransferredGE = {
      geName: currentGE.geName,
      numberOfCourses: newCourseCount,
      units: newUnitCount,
    };
    dispatch(setTransferredGE(updatedGE));
    trpc.transferCredits.setTransferredGE.mutate({ GE: updatedGE });
  };

  const updateNumberOfCourses = (newValue: number) => {
    updateGE(newValue, currentGE.units);
  };

  const updateUnits = (newValue: number) => {
    updateGE(currentGE.numberOfCourses, newValue);
  };

  return (
    <MenuTile title={GE_TITLE_MAP[geName]}>
      <div className="ge-inputs">
        <div className="ge-input-container">
          <p>Number of Courses:</p>
          <GEInput value={currentGE.numberOfCourses} handleUpdate={updateNumberOfCourses} valueType="numberOfCourses" />
        </div>
        <div className="ge-input-container">
          <p>Units Transferred:</p>
          <GEInput value={currentGE.units} handleUpdate={updateUnits} valueType="units" />
        </div>
      </div>
    </MenuTile>
  );
};

const GESection: FC = () => {
  return (
    <MenuSection title="General Education Credits">
      <SectionDescription>
        Enter the GE credits that you've received in each category from other colleges/universities.
      </SectionDescription>
      {ALL_GE_NAMES.map((geName) => (
        <GEMenuTile key={geName} geName={geName} />
      ))}
    </MenuSection>
  );
};

export default GESection;
