import { FC, useState, useContext, useEffect, useCallback } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import Select from 'react-select';
import ThemeContext from '../../../style/theme-context';
import { comboboxTheme } from '../../../helpers/courseRequirements';
import trpc from '../../../trpc';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import {
  setAPExams,
  addUserAPExam,
  removeUserAPExam,
  updateUserExam,
  setUserAPExams,
  UserAPExam,
  setSelectedApRewards,
  updateSelectedApReward,
} from '../../../store/slices/transferCreditsSlice';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import { APExam } from '@peterportal/types';
import './APExamsSection.scss';

interface ScoreSelectionProps {
  score: number;
  setScore: (value: number) => void;
}

interface APExamOption {
  value: APExam;
  label: string;
}

interface RewardsSelectProps {
  selectedIndex?: number;
  options: string[];
  onSelect: (selected: string) => void;
}

// the tree may be array of strings or an object with AND or OR or a mix
type CoursesGrantedTree = string | { AND: CoursesGrantedTree[] } | { OR: CoursesGrantedTree[] };

const ScoreSelection: FC<ScoreSelectionProps> = ({ score, setScore }) => {
  return (
    <div className="select">
      <select
        value={score ?? ''}
        onInput={(event) => setScore(parseInt((event.target as HTMLInputElement).value))}
        className="select-box"
      >
        <optgroup label="Score">
          <option>1</option>
          <option>2</option>
          <option>3</option>
          <option>4</option>
          <option>5</option>
        </optgroup>
      </select>
    </div>
  );
};

const RewardsSelect: FC<RewardsSelectProps> = ({ selectedIndex, options, onSelect }) => {
  return (
    <select value={selectedIndex?.toString()} onChange={(event) => onSelect(event.target.value)} className="select-box">
      <optgroup label="Options">
        <option disabled value="">
          Select...
        </option>
        {options.map((opt, i) => (
          <option key={i} value={opt}>
            {opt}
          </option>
        ))}
      </optgroup>
    </select>
  );
};

const Rewards: FC<{ examName: string; coursesGranted: CoursesGrantedTree }> = ({ examName, coursesGranted }) => {
  const selectedReward = useAppSelector((state) =>
    state.transferCredits.selectedApRewards.find((reward) => reward.examName === examName),
  );
  const dispatch = useAppDispatch();

  const handleSelect = (value: string) => {
    const selectedIndex =
      coursesGranted && typeof coursesGranted === 'object' && 'OR' in coursesGranted
        ? coursesGranted.OR.findIndex((t) => formatCourses(t) === value)
        : 0;

    dispatch(updateSelectedApReward({ examName, path: value, selectedIndex }));
    trpc.transferCredits.updateSelectedAPReward.mutate({ examName, path: value, selectedIndex });
  };

  const formatCourses = (tree: CoursesGrantedTree): string => {
    if (typeof tree === 'string') return tree;
    if ('AND' in tree) return tree.AND.map(formatCourses).join(', ');
    if ('OR' in tree) return tree.OR.map(formatCourses).join(' or ');
    return 'This exam does not clear any courses.';
  };

  const renderTree = (tree: CoursesGrantedTree): React.ReactNode => {
    if (typeof tree === 'string') {
      return <span>{tree}</span>;
    }
    if ('AND' in tree && tree.AND.length > 0) {
      return (
        <span>
          {tree.AND.map((subtree, idx) => (
            <span key={idx}>
              {renderTree(subtree)}
              {idx < tree.AND.length - 1 && ' and '}
            </span>
          ))}
        </span>
      );
    }
    if ('OR' in tree) {
      const options = tree.OR.map(formatCourses);
      return (
        <span style={{ display: 'inline' }}>
          <RewardsSelect selectedIndex={selectedReward?.selectedIndex} options={options} onSelect={handleSelect} />
        </span>
      );
    }
    return <div>This exam does not clear any courses.</div>;
  };

  return <div className="rewards">{renderTree(coursesGranted)}</div>;
};

const APCreditMenuTile: FC<{ userExamInfo: UserAPExam }> = ({ userExamInfo }) => {
  const { examName, score, units } = userExamInfo;
  const updateScore = (value: number) => handleUpdate(value, units);
  const updateUnits = (value: number) => handleUpdate(score, value);
  const apExamInfo = useAppSelector((state) => state.transferCredits.apExamInfo);
  const dispatch = useAppDispatch();

  const selectBox = <ScoreSelection score={score} setScore={updateScore} />;

  const handleUpdate = useCallback(
    async (newScore: number, newUnits: number) => {
      dispatch(updateUserExam({ examName, score: newScore, units: newUnits }));
      trpc.transferCredits.updateUserAPExam.mutate({ examName, score: newScore, units: newUnits });
    },
    [dispatch, examName],
  );

  const deleteFn = useCallback(() => {
    dispatch(removeUserAPExam(examName));
    trpc.transferCredits.deleteUserAPExam.mutate(examName);
  }, [dispatch, examName]);

  const apiExamInfo = apExamInfo.find((exam) => exam.fullName === examName);
  for (const reward of apiExamInfo?.rewards ?? []) {
    if (!reward.acceptableScores.includes(score)) continue;
    const coursesGranted = reward.coursesGranted as CoursesGrantedTree;
    return (
      <MenuTile title={examName} headerItems={selectBox} units={units} setUnits={updateUnits} deleteFn={deleteFn}>
        <Rewards examName={examName} coursesGranted={coursesGranted} />
      </MenuTile>
    );
  }
};

const APExamsSection: FC = () => {
  const isLoggedIn = useIsLoggedIn();
  const dispatch = useAppDispatch();
  const isDark = useContext(ThemeContext).darkMode;
  const apExamInfo = useAppSelector((state) => state.transferCredits.apExamInfo);
  const userAPExams = useAppSelector((state) => state.transferCredits.userAPExams);
  const [examName, setExamName] = useState<string | null>(null);
  const [score, setScore] = useState<number | null>(null);
  const [examsLoading, setExamsLoading] = useState(false);

  // Save initial list of all AP Exams
  const saveAllAPExams = useCallback(
    (apExamInfo: APExam[]) => {
      setExamsLoading(false);
      dispatch(setAPExams(apExamInfo));
    },
    [dispatch],
  );

  // First render, fetch all AP Exams
  useEffect(() => {
    if (apExamInfo.length) return;
    trpc.transferCredits.getAPExamInfo.query().then((allExams) => {
      const processedExams = allExams.map((exam) => ({
        ...exam,
        catalogueName: exam.catalogueName ?? null,
      }));
      saveAllAPExams(processedExams);
    });
  }, [dispatch, apExamInfo.length, saveAllAPExams]);

  // Save AP Exam to store
  useEffect(() => {
    if (!isLoggedIn || !examName || !score) return;
    const examInfo = apExamInfo.find((exam) => exam.fullName === examName);
    const units = examInfo?.rewards?.find((r) => r.acceptableScores.includes(score))?.unitsGranted ?? 0;

    if (!userAPExams.find((exam) => exam.examName === examName)) {
      dispatch(addUserAPExam({ examName, score, units }));
      trpc.transferCredits.addUserAPExam.mutate({ examName, score, units });
    }
    // Remove exam from select options
    setExamName(null);
    setScore(null);
  }, [dispatch, examName, score, apExamInfo, userAPExams, isLoggedIn, examsLoading]);

  // Fetch saved AP exams and rewards and save to store
  useEffect(() => {
    trpc.transferCredits.getSavedAPExams.query().then((savedExams) => {
      dispatch(setUserAPExams(savedExams));
    });
    trpc.transferCredits.getSelectedAPRewards.query().then((rewards) => {
      dispatch(setSelectedApRewards(rewards));
    });
  }, [dispatch]);

  const baseSelectOptions: APExamOption[] = apExamInfo.map((exam) => ({
    value: exam,
    label: exam.fullName,
  }));

  const apSelectOptions = baseSelectOptions.filter((exam) => {
    return !userAPExams.some((userExam) => userExam.examName === exam.label);
  });

  return (
    <MenuSection title="AP Exam Credits">
      <SectionDescription>
        Enter the names of AP Exams that you&rsquo;ve taken to clear course prerequisites.
      </SectionDescription>
      {userAPExams.map((exam) => (
        <APCreditMenuTile key={exam.examName} userExamInfo={exam} />
      ))}
      <div className="input">
        <div className="exam-input">
          <Select
            className="ppc-combobox"
            classNamePrefix="ppc-combobox"
            value={apSelectOptions.find((opt) => opt.label === examName) ?? null}
            options={apSelectOptions}
            isSearchable
            onChange={(selectedOption) => setExamName(selectedOption?.label || null)}
            placeholder="Add an AP Exam..."
            theme={(t) => comboboxTheme(t, isDark)}
          />
        </div>
        <div className="score-input">
          <Select
            className="ppc-combobox"
            classNamePrefix="ppc-combobox"
            value={score ? { value: score, label: score.toString() } : null}
            options={[
              { value: 1, label: '1' },
              { value: 2, label: '2' },
              { value: 3, label: '3' },
              { value: 4, label: '4' },
              { value: 5, label: '5' },
            ]}
            onChange={(selectedOption) => setScore(selectedOption?.value || null)}
            placeholder="Score"
            theme={(t) => comboboxTheme(t, isDark)}
          />
        </div>
      </div>
    </MenuSection>
  );
};

export default APExamsSection;
