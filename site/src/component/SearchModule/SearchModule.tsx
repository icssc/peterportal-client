import { useState, useEffect, FC, useRef, useMemo } from 'react';
import './SearchModule.scss';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { SearchIndex, SearchResultData } from '../../types/types';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';
import { setShowSavedCourses } from '../../store/slices/roadmapSlice';
import trpc from '../../trpc.ts';
import { setQuery, setResults } from '../../store/slices/searchSlice';
import { transformGQLData } from '../../helpers/util';

import SearchIcon from '@mui/icons-material/Search';
import CheckIcon from '@mui/icons-material/Check';
import MenuItem from '@mui/material/MenuItem';
import FormControl from '@mui/material/FormControl';
import ListItemText from '@mui/material/ListItemText';
import Select, { SelectChangeEvent } from '@mui/material/Select';
import Autocomplete from '@mui/material/Autocomplete';
import TextField from '@mui/material/TextField';
import { Icon } from '@mui/material';

const SEARCH_TIMEOUT_MS = 300;

interface SearchModuleProps {
  index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const search = useAppSelector((state) => state.search[index]);
  const [searchQuery, setSearchQuery] = useState('');
  const [pendingRequest, setPendingRequest] = useState<number | null>(null);
  const abortControllerRef = useRef<AbortController | null>(null);

  // fuzzySearch is defined after filter state/lookup tables so it can depend on them safely

  // Cleanup abort controller on unmount
  useEffect(() => {
    const controller = abortControllerRef.current;
    return () => {
      controller?.abort();
    };
  }, []);

  const searchImmediately = (query: string) => {
    if (pendingRequest) clearTimeout(pendingRequest);
    if (location.pathname === '/roadmap') {
      dispatch(setShowSavedCourses(!query));
    }
    if (query && query !== search.query) {
      dispatch(setQuery({ index, query }));
      setPendingRequest(null);
    }
  };
  const searchAfterTimeout = (query: string) => {
    setSearchQuery(query);
    if (pendingRequest) clearTimeout(pendingRequest);
    const timeout = window.setTimeout(() => searchImmediately(query), SEARCH_TIMEOUT_MS);
    setPendingRequest(timeout);
  };

  const coursePlaceholder = 'Search for a course...';
  const professorPlaceholder = 'Search a professor';
  const placeholder = index === 'courses' ? coursePlaceholder : professorPlaceholder;

  // Set the height of the tag selector based on the number of tags
  const ITEM_HEIGHT = 48;
  const ITEM_PADDING_TOP = 8;
  const MenuProps = {
    PaperProps: {
      style: {
        maxHeight: ITEM_HEIGHT * 4.5 + ITEM_PADDING_TOP,
        width: 250,
      },
    },
  };

  // Keep track of selected options for the level and GE filters and handle changes
  const [selectedLevels, setSelectedLevels] = useState<string[]>([]);
  const [selectedGECategories, setSelectedGECategories] = useState<string[]>([]);
  const [selectedDepartments, setSelectedDepartments] = useState<string[]>([]);

  const handleLevelSelection = (event: SelectChangeEvent<typeof selectedLevels>) => {
    const {
      target: { value },
    } = event;
    setSelectedLevels(
      // On autofill we get a stringified value.
      typeof value === 'string' ? value.split(',') : value,
    );
  };

  const handleGeCategorySelection = (event: SelectChangeEvent<typeof selectedGECategories>) => {
    const {
      target: { value },
    } = event;
    setSelectedGECategories(
      // On autofill we get a stringified value.
      typeof value === 'string' ? value.split(',') : value,
    );
  };

  // Lookup tables for mapping internal codes to user-friendly display names.
  const levels = useMemo<Record<string, string>>(
    () => ({
      LowerDiv: 'Lower Division',
      UpperDiv: 'Upper Division',
      Graduate: 'Graduate',
    }),
    [],
  );

  const departments: Record<string, string> = {
    'AC ENG': 'Academic English and ESL',
    AFAM: 'African American Studies',
    ANATOMY: 'Anatomy and Neurobiology',
    ANESTH: 'Anesthesiology',
    ANTHRO: 'Anthropology',
    ARABIC: 'Arabic',
    ARMN: 'Armenian',
    ART: 'Art',
    'ART HIS': 'Art History',
    ARTS: 'Arts',
    ARTSHUM: 'Arts and Humanities',
    ASIANAM: 'Asian American Studies',
    BANA: 'Business Analytics',
    BATS: 'Biomedical and Translational Science',
    'BIO SCI': 'Biological Sciences',
    BIOCHEM: 'Biological Chemistry',
    BME: 'Biomedical Engineering',
    CAMPREC: 'Campus Recreation',
    CBE: 'Chemical and Biomolecular Engineering',
    CEM: 'Community and Environmental Medicine',
    'CHC/LAT': 'Chicano Latino',
    CHEM: 'Chemistry',
    CHINESE: 'Chinese',
    CLASSIC: 'Classics',
    'CLT&THY': 'Culture & Theory',
    COGS: 'Cognitive Sciences',
    'COM LIT': 'Comparative Literature',
    COMPSCI: 'Computer Science',
    CRITISM: 'Criticism',
    'CRM/LAW': 'Criminology, Law and Society',
    CSE: 'Computer Science and Engineering',
    DANCE: 'Dance',
    DERM: 'Dermatology',
    'DEV BIO': 'Developmental and Cell Biology',
    DRAMA: 'Drama',
    EARTHSS: 'Earth System Science',
    EAS: 'East Asian Studies',
    'ECO EVO': 'Ecology and Evolutionary Biology',
    ECON: 'Economics',
    ECPS: 'Embedded and Cyber-Physical Systems',
    'ED AFF': 'Educational Affairs (Sch of Med)',
    EDUC: 'Education',
    EECS: 'Electrical Engineering & Computer Science',
    EHS: 'Environmental Health Sciences',
    ENGLISH: 'English',
    ENGR: 'Engineering',
    ENGRCEE: 'Engineering, Civil and Environmental',
    ENGRMAE: 'Engineering, Mechanical and Aerospace',
    EPIDEM: 'Epidemiology',
    'ER MED': 'Emergency Medicine',
    'EURO ST': 'European Studies',
    'FAM MED': 'Family Medicine',
    FIN: 'Finance',
    'FLM&MDA': 'Film and Media Studies',
    FRENCH: 'French',
    GDIM: 'Game Design and Interactive Media',
    'GEN&SEX': 'Gender and Sexuality Studies',
    GERMAN: 'German',
    'GLBL ME': 'Global Middle East Studies',
    GLBLCLT: 'Global Cultures',
    GREEK: 'Greek',
    HEBREW: 'Hebrew',
    HINDI: 'Hindi',
    HISTORY: 'History',
    HUMAN: 'Humanities',
    HUMARTS: 'Humanities and Arts',
    'I&C SCI': 'Information and Computer Science',
    IN4MATX: 'Informatics',
    INNO: 'Masters of Innovation and Entrepreneurship',
    'INT MED': 'Internal Medicine',
    'INTL ST': 'International Studies',
    IRAN: 'Iranian',
    ITALIAN: 'Italian',
    JAPANSE: 'Japanese',
    KOREAN: 'Korean',
    LATIN: 'Latin',
    LAW: 'Law',
    'LIT JRN': 'Literary Journalism',
    LPS: 'Logic and Philosophy of Science',
    LSCI: 'Language Science',
    'M&MG': 'Microbiology and Molecular Genetics',
    MATH: 'Mathematics',
    MED: 'Medicine',
    'MED ED': 'Medical Education',
    'MED HUM': 'Medical Humanities',
    MGMT: 'Management',
    'MGMT EP': 'Executive MBA',
    'MGMT FE': 'Fully Employed MBA',
    'MGMT HC': 'Health Care MBA',
    MGMTMBA: 'Management MBA',
    MGMTPHD: 'Management PhD',
    'MIC BIO': 'Microbiology',
    'MOL BIO': 'Molecular Biology and Biochemistry',
    MPAC: 'Accounting',
    MSE: 'Materials Science and Engineering',
    MUSIC: 'Music',
    'NET SYS': 'Networked Systems',
    NEURBIO: 'Neurobiology and Behavior',
    NEUROL: 'Neurology',
    'NUR SCI': 'Nursing Science',
    'OB/GYN': 'Obstetrics and Gynecology',
    OPHTHAL: 'Ophthalmology',
    PATH: 'Pathology and Laboratory Medicine',
    'PED GEN': 'Pediatrics Genetics',
    PEDS: 'Pediatrics',
    PERSIAN: 'Persian',
    PHARM: 'Medical Pharmacology',
    PHILOS: 'Philosophy',
    PHRMSCI: 'Pharmaceutical Sciences',
    'PHY SCI': 'Physical Science',
    PHYSICS: 'Physics',
    PHYSIO: 'Physiology and Biophysics',
    PLASTIC: 'Plastic Surgery',
    'PM&R': 'Physical Medicine and Rehabilitation',
    'POL SCI': 'Political Science',
    PORTUG: 'Portuguese',
    PSCI: 'Psychological Science',
    PSYCH: 'Psychology',
    'PUB POL': 'Public Policy',
    PUBHLTH: 'Public Health',
    RADIO: 'Radiology',
    'REL STD': 'Religious Studies',
    ROTC: "Reserve Officers' Training Corps",
    RUSSIAN: 'Russian',
    'SOC SCI': 'Social Science',
    SOCECOL: 'Social Ecology',
    SOCIOL: 'Sociology',
    SPANISH: 'Spanish',
    SPPS: 'Social Policy & Public Service',
    STATS: 'Statistics',
    SURGERY: 'Surgery',
    SWE: 'Software Engineering',
    TAGALOG: 'Tagalog',
    TOX: 'Toxicology',
    UCDC: 'UC Washington DC',
    'UNI AFF': 'University Affairs',
    'UNI STU': 'University Studies',
    UPPP: 'Urban Planning and Public Policy',
    VIETMSE: 'Vietnamese',
    'VIS STD': 'Visual Studies',
    WRITING: 'Writing',
  };

  const geCategories = useMemo<Record<string, string>>(
    () => ({
      'GE-1A': 'Lower Division Writing',
      'GE-1B': 'Upper Division Writing',
      'GE-2': 'Science and Technology',
      'GE-3': 'Social and Behavioral Sciences',
      'GE-4': 'Arts and Humanities',
      'GE-5A': 'Quantitative Literacy',
      'GE-5B': 'Formal Reasoning',
      'GE-6': 'Language Other Than English',
      'GE-7': 'Multicultural Studies',
      'GE-8': 'International/Global Issues',
    }),
    [],
  );

  // Run search when query, page, or filters change
  useEffect(() => {
    // Cancel any in-flight request
    abortControllerRef.current?.abort();
    const abortController = new AbortController();
    abortControllerRef.current = abortController;

    const run = async () => {
      try {
        // Only apply filters for course searches
        const selectedCourseLevelCodes = (() => {
          if (index !== 'courses' || selectedLevels.length === 0) return undefined;
          const entry = Object.entries(levels).find(([, label]) => label === selectedLevels.join(','));
          return entry?.[0];
        })();
        const selectedGeCodes = (() => {
          if (index !== 'courses' || selectedGECategories.length === 0) return undefined;
          const entry = Object.entries(geCategories).find(([, label]) => label === selectedGECategories.join(','));
          return entry?.[0];
        })();
        const departmentCodes =
          index === 'courses' && selectedDepartments.length > 0 ? selectedDepartments.join(',') : undefined;

        const base = {
          query: search.query,
          take: NUM_RESULTS_PER_PAGE,
          skip: NUM_RESULTS_PER_PAGE * search.pageNumber,
          resultType: index === 'courses' ? 'course' : 'instructor',
        } as const;
        const payload = {
          ...base,
          ...(departmentCodes ? { department: departmentCodes } : {}),
          ...(selectedCourseLevelCodes ? { courseLevel: selectedCourseLevelCodes } : {}),
          ...(selectedGeCodes ? { ge: selectedGeCodes } : {}),
        } as Parameters<typeof trpc.search.get.query>[0];

        const { count, results } = await trpc.search.get.query(payload, { signal: abortController.signal });

        if (!abortController.signal.aborted) {
          dispatch(
            setResults({
              index,
              results: results.map((x) => transformGQLData(index, x.result)) as SearchResultData,
              count,
            }),
          );
        }
      } catch (error) {
        if (error instanceof Error && error.name !== 'AbortError') {
          console.error('Search error:', error);
        }
      }
    };

    run();

    // Re-run when query/page or any selected filter changes
  }, [
    dispatch,
    geCategories,
    index,
    levels,
    search.pageNumber,
    search.query,
    selectedDepartments,
    selectedGECategories,
    selectedLevels,
  ]);

  return (
    <div className="search-module">
      <Form.Group className="form-group">
        <InputGroup>
          <Form.Control
            className="search-bar"
            aria-label="search"
            type="search"
            placeholder={placeholder}
            onChange={(e) => searchAfterTimeout(e.target.value)}
            defaultValue={search.query}
            autoCorrect="off"
          />
          <button className="input-group-text" onClick={() => searchImmediately(searchQuery)}>
            <SearchIcon />
          </button>
        </InputGroup>
      </Form.Group>
      <div className="filter-group">
        <FormControl sx={{ width: 300 }}>
          <Select
            id="level-select"
            multiple
            value={selectedLevels}
            onChange={handleLevelSelection}
            displayEmpty
            renderValue={(selected) => {
              return selected.length === 0 ? (
                <p className="filter-placeholder">Filter by course level...</p>
              ) : (
                selected.join(', ')
              );
            }}
            MenuProps={MenuProps}
            size="small"
          >
            {Object.keys(levels).map((key) => (
              <MenuItem key={key} value={levels[key]}>
                {selectedLevels.includes(levels[key]) ? <CheckIcon /> : <span style={{ width: 24 }}></span>}
                <ListItemText primary={levels[key]} />
              </MenuItem>
            ))}
          </Select>
        </FormControl>
        <FormControl sx={{ width: 300 }}>
          <Select
            size="small"
            id="ge-category-select"
            multiple
            value={selectedGECategories}
            onChange={handleGeCategorySelection}
            displayEmpty
            renderValue={(selected) => {
              return selected.length === 0 ? (
                <p className="filter-placeholder">Filter by GE category...</p>
              ) : (
                selected.join(', ')
              );
            }}
            MenuProps={MenuProps}
          >
            {Object.keys(geCategories).map((key) => (
              <MenuItem key={key} value={geCategories[key]}>
                {selectedGECategories.includes(geCategories[key]) ? <CheckIcon /> : <Icon />}
                <ListItemText primary={`${key}: ${geCategories[key]}`} />
              </MenuItem>
            ))}
          </Select>
        </FormControl>
        <Autocomplete
          multiple
          size="small"
          limitTags={2}
          id="multiple-limit-tags"
          options={Object.keys(departments)}
          value={selectedDepartments}
          onChange={(_event, newValue) => {
            setSelectedDepartments(newValue);
          }}
          getOptionLabel={(option) => departments[option]}
          renderInput={(params) => (
            <TextField {...params} placeholder={selectedDepartments.length === 0 ? 'Search departments...' : ''} />
          )}
          sx={{ flexGrow: 1, height: 60 }}
        />
      </div>
    </div>
  );
};

export default SearchModule;
