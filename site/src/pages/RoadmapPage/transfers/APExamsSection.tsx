import { FC, useState, useContext, useEffect, useCallback } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import Select from 'react-select';
import ThemeContext from '../../../style/theme-context';
import { comboboxTheme } from '../../../helpers/courseRequirements';
import trpc from '../../../trpc';
import { useAppDispatch } from '../../../store/hooks';

interface ScoreSelectionProps {
  score: number | null;
  setScore: (value: number) => void;
}

interface APExam {
  fullName: string;
  catalogueName: string;
  rewards: {
    acceptableScores: number[];
    unitsGranted: number;
    electiveUnitsGranted: number;
    geCategories: string[];
    coursesGranted: {
      string: string[];
    };
  }[];
}

const ScoreSelection: FC<ScoreSelectionProps> = ({ score, setScore }) => {
  /** @todo style this according to the Figma, add options & optgroup titled 'Score' */
  return (
    <select value={score ?? ''} onInput={(event) => setScore(parseInt((event.target as HTMLInputElement).value))}>
      <optgroup label="Score">
        <option value="1">1</option>
        <option value="2">2</option>
        <option value="3">3</option>
        <option value="4">4</option>
        <option value="5">5</option>
      </optgroup>
    </select>
  );
};

const APCreditMenuTile: FC = () => {
  const [score, setScore] = useState<number | null>(null);
  const [units, setUnits] = useState<number>(0);

  const selectBox = <ScoreSelection score={score} setScore={setScore} />;
  const deleteFn = () => {};

  return (
    <MenuTile title="AP Sample Name" headerItems={selectBox} units={units} setUnits={setUnits} deleteFn={deleteFn}>
      <p>A list of courses cleared by this exam</p>
    </MenuTile>
  );
};

const APExamsSection: FC = () => {
  const dispatch = useAppDispatch();
  const isDark = useContext(ThemeContext).darkMode;

  const saveAPExams = useCallback((APExams: APExam[]) => {
    console.log('save exams', typeof APExams);
  }, []);

  useEffect(() => {
    trpc.transferCredits.getAPExamInfo.query().then((fetchedExams) => {
      saveAPExams(fetchedExams);
      console.log('fetched exams', typeof fetchedExams);
    });
  }, [dispatch, saveAPExams]);

  //const APSelectOptions = ;

  return (
    <MenuSection title="AP Exam Credits">
      <SectionDescription>
        Enter the names of AP Exams that you&rsquo;ve taken to clear course prerequisites.
      </SectionDescription>
      {/** @todo map each user AP Exam to one of these tiles */}
      <APCreditMenuTile />
      <div style={{ display: 'flex', alignItems: 'center', gap: '1rem' }}>
        <div style={{ flex: '1' }}>
          <Select placeholder="Add an AP Exam..." theme={(t) => comboboxTheme(t, isDark)} />
        </div>
        <div style={{ flex: '0.5' }}>
          <Select placeholder="Score" theme={(t) => comboboxTheme(t, isDark)} />
        </div>
      </div>
    </MenuSection>
  );
};

export default APExamsSection;
