import './GESection.scss';
import { FC, useState, useEffect } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import { GEName, GETitle, TransferredGE } from '@peterportal/types';
import trpc from '../../../trpc';
import { setAllTransferredGEs, setTransferredGE } from '../../../store/slices/transferCreditsSlice';
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

/*

Questions:
- Are the GE input values going to be updated based on other categories? E.g. if a user gets one GE course from an AP exam, will that course be shown in the corresponding GE in the GE menu?
- Do I need to use the useIsLoggedIn() hook?
- Should TransferredGE[] be a map instead of an array?

*/

interface GEInputProps {
  value: number;
  handleUpdate: (value: number) => void;
}

const GEInput: FC<GEInputProps> = ({ value, handleUpdate }) => {
  const onBlur = (e: React.FocusEvent<HTMLInputElement>) => {
    const newValue = parseInt(e.target.value);
    if (isNaN(newValue)) {
      e.target.value = value.toString();
      return;
    }
    handleUpdate(newValue);
  };
  return <input className="ge-input" type="number" min="0" step="1" defaultValue={value} onBlur={onBlur} />;
};

interface GEMenuTileProps {
  geName: GEName;
}

const GEMenuTile: FC<GEMenuTileProps> = ({ geName }) => {
  const dispatch = useAppDispatch();
  const transferredGEs = useAppSelector((state) => state.transferCredits.transferredGEs);
  const [numberOfCourses, setNumberOfCourses] = useState<number>(0);
  const [units, setUnits] = useState<number>(0);

  useEffect(() => {
    const geData = transferredGEs.find((ge) => ge.geName === geName);
    setNumberOfCourses(geData ? geData.numberOfCourses : 0);
    setUnits(geData ? geData.units : 0);
  }, [geName, transferredGEs]);

  const updateGE = (newNumberOfCourses: number, newUnits: number) => {
    const GE: TransferredGE = {
      geName: geName,
      numberOfCourses: newNumberOfCourses,
      units: newUnits,
    };
    dispatch(setTransferredGE(GE));
    trpc.transferCredits.setTransferredGE.mutate({ GE });
  };

  const updateNumberOfCourses = (newNumberOfCourses: number) => {
    if (newNumberOfCourses === numberOfCourses) return;
    setNumberOfCourses(newNumberOfCourses);
    updateGE(newNumberOfCourses, units);
  };

  const updateUnits = (newUnits: number) => {
    if (newUnits === units) return;
    setUnits(newUnits);
    updateGE(numberOfCourses, newUnits);
  };

  return (
    <MenuTile title={GE_TITLE_MAP[geName]}>
      <div className="ge-inputs">
        <div className="ge-input-container">
          <p>Number of Courses:</p>
          <GEInput value={numberOfCourses} handleUpdate={updateNumberOfCourses} />
        </div>
        <div className="ge-input-container">
          <p>Units Transferred:</p>
          <GEInput value={units} handleUpdate={updateUnits} />
        </div>
      </div>
    </MenuTile>
  );
};

const GESection: FC = () => {
  const dispatch = useAppDispatch();

  useEffect(() => {
    trpc.transferCredits.getTransferredGEs.query().then((transferredGEs) => {
      dispatch(setAllTransferredGEs(transferredGEs));
    });
  }, [dispatch]);

  return (
    <MenuSection title="General Education Credits">
      <SectionDescription>GE Section Description</SectionDescription>
      {ALL_GE_NAMES.map((geName) => (
        <GEMenuTile key={geName} geName={geName} />
      ))}
    </MenuSection>
  );
};

export default GESection;
