import MuiPagination from '@material-ui/lab/Pagination';
import { Dispatch, SetStateAction } from 'react';
import styled from 'styled-components';
import breakpoints from '../themes/breakpoints';
import { useWindowDimensions } from '../util';

interface PaginationProps {
  total: number;
  page: number;
  first: number;
  setPage: Dispatch<SetStateAction<number>>;
  setSkip: Dispatch<SetStateAction<number>>;
}

// const SetGalleryWrapper = styled.div<SetGalleryWrapperProps>(({ setsPerRow = 4, galleryWidth = 100, theme }) => ({

const Pagination: React.FC<PaginationProps> = ({ total, page, first, setPage, setSkip }) => {
  const count = Math.ceil(total / first);

  const { width } = useWindowDimensions();

  return (
    <StyledPagination
      count={count}
      page={page}
      onChange={(event: React.ChangeEvent<unknown>, value: number) => {
        setPage(value);
        setSkip((value - 1) * first);
      }}
      size={width > breakpoints.lg ? 'medium' : 'small'}
      siblingCount={width > breakpoints.lg ? 2 : 0}
    />
  );
};

const StyledPagination = styled(MuiPagination)(({ theme }) => ({
  textAlign: 'center',
  display: 'inline-block',
}));

export default Pagination;
