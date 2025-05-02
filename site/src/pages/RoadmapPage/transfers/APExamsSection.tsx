import { FC, useState, useContext, useEffect, useCallback } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import Select from 'react-select';
import ThemeContext from '../../../style/theme-context';
import { comboboxTheme } from '../../../helpers/courseRequirements';
import trpc from '../../../trpc';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { addUserAPExam, setAPExams } from '../../../store/slices/transferCreditsSlice';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';

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

interface userAPExam {
  examName: string;
  score: number;
  units: number;
}

interface APExamOption {
  value: APExam;
  label: string;
}

interface APCreditMenuTileProps {
  examName: string;
  userScore: number | null;
  userUnits: number;
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

const APCreditMenuTile: FC<APCreditMenuTileProps> = ({ examName, userScore, userUnits }) => {
  // For each saved AP Exam, create a menu tile
  const [score, setScore] = useState<number | null>(null);
  const [units, setUnits] = useState<number>(0);
  setScore(userScore);
  setUnits(userUnits);

  const selectBox = <ScoreSelection score={score} setScore={setScore} />;
  const deleteFn = () => {};

  return (
    <MenuTile title={examName} headerItems={selectBox} units={units} setUnits={setUnits} deleteFn={deleteFn}>
      <p>A list of courses cleared by this exam</p>
    </MenuTile>
  );
};

const APExamsSection: FC = () => {
  const isLoggedIn = useIsLoggedIn;
  const dispatch = useAppDispatch();
  const isDark = useContext(ThemeContext).darkMode;
  const APExams = useAppSelector((state) => state.transferCredits.APExams);
  const userAPExams = useAppSelector((state) => state.transferCredits.userAPExams);
  const [currentExam, setCurrentExam] = useState<string | null>(null);
  const [currentScore, setCurrentScore] = useState<number | null>(null);

  // Save initial list of all AP Exams
  const saveAllAPExams = useCallback(
    (APExams: APExam[]) => {
      dispatch(setAPExams(APExams));
    },
    [dispatch],
  );

  // First render, fetch all AP Exams
  useEffect(() => {
    if (APExams.length) return;
    trpc.transferCredits.getAPExamInfo.query().then((allExams) => {
      saveAllAPExams(allExams);
    });
  }, [dispatch, APExams.length, saveAllAPExams]);

  // Save AP Exams for user
  const saveUserAPExams = useCallback(
    (examsToSave: userAPExam[]) => {
      if (!isLoggedIn) return;
      trpc.transferCredits.saveSelectedAPExams.mutate({ apExams: examsToSave });
    },
    [isLoggedIn],
  );

  useEffect(() => {
    const examName = currentExam;
    const score = currentScore;
    if (!examName || !score) return;
    const foundUnits = APExams.find((exam) => exam.fullName === examName)?.rewards[0].unitsGranted ?? 0;
    saveUserAPExams([{ examName, score, units: foundUnits }]);
  }, [APExams, currentExam, currentScore, saveUserAPExams]);

  // Get user's AP Exams and save to state
  // Fetch data for those exams
  // render menu tiles for user's AP Exams
  // On change, (useCallback?) re-render and save

  useEffect(() => {
    trpc.transferCredits.getSavedAPExams.query().then((savedExams) => {
      for (const exam of savedExams) {
        dispatch(addUserAPExam(exam));
      }
    });
  }, [dispatch]);

  const APSelectOptions: APExamOption[] = APExams.map((exam) => ({
    value: exam,
    label: exam.fullName,
  }));

  return (
    <MenuSection title="AP Exam Credits">
      <SectionDescription>
        Enter the names of AP Exams that you&rsquo;ve taken to clear course prerequisites.
      </SectionDescription>
      {/** @todo map each user AP Exam to one of these tiles */}
      {userAPExams.map((exam) => (
        <APCreditMenuTile key={exam.examName} examName={exam.examName} userScore={exam.score} userUnits={exam.units} />
      ))}
      {/** @todo add a button to add a new AP Exam */}
      <div style={{ display: 'flex', alignItems: 'center', gap: '1rem' }}>
        <div style={{ flex: '1' }}>
          <Select
            options={APSelectOptions}
            isSearchable
            onChange={(selectedOption) => setCurrentExam(selectedOption?.label || null)}
            placeholder="Add an AP Exam..."
            theme={(t) => comboboxTheme(t, isDark)}
          />
        </div>
        <div style={{ flex: '0.5' }}>
          <Select
            options={[
              { value: 1, label: '1' },
              { value: 2, label: '2' },
              { value: 3, label: '3' },
              { value: 4, label: '4' },
              { value: 5, label: '5' },
            ]}
            onChange={(selectedOption) => setCurrentScore(selectedOption?.value || null)}
            placeholder="Score"
            theme={(t) => comboboxTheme(t, isDark)}
          />
        </div>
      </div>
    </MenuSection>
  );
};

export default APExamsSection;
