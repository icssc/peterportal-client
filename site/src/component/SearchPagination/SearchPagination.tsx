import { FC } from "react";
import { Pagination } from "react-bootstrap";
import { useAppDispatch, useAppSelector } from "src/store/hooks";
import { setPageNumber } from "src/store/slices/searchSlice";
import { SearchIndex } from "src/types/types";

interface SearchPaginationProps {
  index: SearchIndex;
}

// TODO: Make this a global constant (also in ../SearchModule/SearchModule.tsx)
const MAX_PAGE_NUMBER = 5;

const SearchPagination: FC<SearchPaginationProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const coursePageNumber = useAppSelector(state => state.search.courses.pageNumber);
  const professorPageNumber = useAppSelector(state => state.search.professors.pageNumber);

  const clickPageNumber = (pageNumber: number) => {
    dispatch(setPageNumber({index, pageNumber}));
  }

  const active = index === 'courses' ? coursePageNumber : professorPageNumber;
  let items = [];
  for (let i = 0; i < MAX_PAGE_NUMBER; i++) {
    items.push(
      <Pagination.Item key={i} active={i === active} onClick={() => clickPageNumber(i)}>
        {i + 1}
      </Pagination.Item>,
    );
  }

  return <Pagination size="lg">{items}</Pagination>
}

export default SearchPagination;