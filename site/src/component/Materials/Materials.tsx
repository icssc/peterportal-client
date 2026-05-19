import { FC, useState, useEffect, useCallback, useMemo } from 'react';
import './Materials.scss';
import { Chip, Tooltip } from '@mui/material';

import { useAppSelector } from '../../store/hooks';
import trpc from '../../trpc';

import { MenuItem, Select } from '@mui/material';
import InfoOutlineIcon from '@mui/icons-material/InfoOutline';
import Toast, { ToastSeverity } from '../../helpers/toast';
import Link from 'next/link';

interface MaterialsProps {
  courseID: string;
  termsOffered?: string[];
}

export interface MaterialsEntry {
  instructors: string[];
  requirement: string | null;
  title: string;
  link: string | null;
  author: string | null;
  format: string;
  isbn: string | null;
}

export type MaterialsData = Record<string, MaterialsEntry[]>;

const Materials: FC<MaterialsProps> = (props) => {
  const courseIDSplit = props.courseID.split(' ');
  const department = courseIDSplit.slice(0, courseIDSplit.length - 1).join(' ');
  const courseNumber = courseIDSplit[courseIDSplit.length - 1];

  const [materialsData, setMaterialsData] = useState<MaterialsData | null>(null);
  const [terms, setTerms] = useState<string[] | null>(null);

  const fetchMaterialsFromAPI = useCallback(async () => {
    const res = await trpc.courseMaterials.get.query({
      department: department,
      courseNumber: courseNumber,
    });

    try {
      const data: MaterialsData = {};
      res.forEach((material) => {
        const term = material.year + ' ' + material.quarter;
        const entry: MaterialsEntry = {
          instructors: material.instructors,
          requirement: material.requirement,
          title: material.title,
          link: material.link,
          author: material.author,
          format: material.format,
          isbn: material.isbn,
        };
        if (!data[term]) {
          data[term] = [];
        }
        data[term].push(entry);
      });
      setMaterialsData(data);
      setTerms(Object.keys(data ?? {}));
    } catch (error) {
      if (error instanceof TypeError) {
        setMaterialsData({});
        setTerms([]);
      }
    }
  }, [department, courseNumber]);

  useEffect(() => {
    fetchMaterialsFromAPI();
  }, [fetchMaterialsFromAPI]);

  const currentQuarter = useAppSelector((state) => state.schedule.currentQuarter);
  const [selectedQuarter, setSelectedQuarter] = useState(terms ? terms[0] : currentQuarter);

  const [showToast, setShowToast] = useState(false);
  const [toastMsg, setToastMsg] = useState('');
  const [toastSeverity, setToastSeverity] = useState<ToastSeverity>('success');

  const handleClose = () => {
    setShowToast(false);
  };

  const materials = useMemo(() => {
    if (!materialsData || selectedQuarter === '') return [];
    return materialsData[selectedQuarter] ?? [];
  }, [materialsData, selectedQuarter]);

  if (terms && terms.length <= 0) {
    return (
      <div className="materials-no-data">
        <i>
          No material data is available for {department} {courseNumber}.
        </i>
      </div>
    );
  }

  const renderData = (data: MaterialsEntry, index: number) => {
    const clicktoCopy = (event: React.MouseEvent<HTMLElement>, sectionCode: string) => {
      event.stopPropagation();
      navigator.clipboard.writeText(sectionCode);
      setToastMsg('ISBN copied to clipboard');
      setToastSeverity('success');
      setShowToast(true);
    };

    //This function returns the data for a dynamic table after accessing the API
    return (
      <tr key={index}>
        <td className="data-col">{data.instructors.join('\n')}</td>
        <td className="data-col">{data.requirement ? data.requirement : 'N/A'}</td>
        <td className="data-col">
          {data.link ? (
            <Link href={data.link} rel="noopener noreferrer" target="_blank">
              {data.title}
            </Link>
          ) : (
            data.title
          )}
        </td>
        <td className="data-col">{data.author ? data.author : 'N/A'}</td>
        <td className="data-col">{data.format}</td>
        <td className="data-col">
          {data.isbn ? (
            <Tooltip title="Click to copy ISBN">
              <Chip
                label={data.isbn}
                onClick={(e) => {
                  clicktoCopy(e, data.isbn!);
                }}
              />
            </Tooltip>
          ) : (
            'N/A'
          )}
        </td>
      </tr>
    );
  };

  if (!materials) {
    return <p> Loading Materials..</p>;
  } else {
    const materialElements: JSX.Element[] = [];

    const sortedMaterials = [...materials].sort((a, b) => a.title.localeCompare(b.title));
    sortedMaterials.forEach((material, i) => {
      materialElements.push(renderData(material, i));
    });

    const termOptions =
      props.termsOffered?.map((term) => {
        return { text: term, value: term };
      }) ?? [];

    const libraryLink = 'https://www.lib.uci.edu/affordable-initiatives/course-materials';

    return (
      <div>
        <div className="library-link">
          <InfoOutlineIcon fontSize="small" />
          <p>
            <i>
              Detailed information is available on the{' '}
              <Link href={libraryLink} rel="noopener noreferrer" target="_blank">
                UC Irvine Libraries website
              </Link>
              .
            </i>
          </p>
        </div>

        <Toast text={toastMsg} severity={toastSeverity} showToast={showToast} onClose={handleClose} />

        {props.termsOffered ? (
          <Select
            value={selectedQuarter ?? currentQuarter}
            onChange={(e) => setSelectedQuarter(e.target.value)}
            renderValue={() => {
              return selectedQuarter;
            }}
          >
            {termOptions.map((opt) => (
              <MenuItem key={opt.value} value={opt.value}>
                {opt.text}
              </MenuItem>
            ))}
          </Select>
        ) : (
          <div className="materials-quarter">Showing results for {selectedQuarter}</div>
        )}
        <div className="table-wrapper">
          <table className="ppc-table materials-table">
            <thead>
              <tr>
                <th>Instructor</th>
                <th>Requirement</th>
                <th>Title</th>
                <th>Author</th>
                <th>Format</th>
                <th>ISBN</th>
              </tr>
            </thead>
            <tbody>{materialElements}</tbody>
          </table>
        </div>
      </div>
    );
  }
};

export default Materials;
