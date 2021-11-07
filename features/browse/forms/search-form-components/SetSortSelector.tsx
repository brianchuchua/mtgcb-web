import MenuItem from '@material-ui/core/MenuItem';
import Select from '@material-ui/core/Select';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { RootState } from '../../../../redux/rootReducer';
import { expansionSortByOptions, setExpansionSort, setExpansionSortDirection } from '../../browseSlice';

const SetSortSelector: React.FC = () => {
  const dispatch = useDispatch();
  const { sortExpansionBy, sortExpansionByDirection } = useSelector((state: RootState) => state.browse);

  const handleSortByChange = (event: React.ChangeEvent<{ value: string }>) => {
    dispatch(setExpansionSort(event.target.value));
  };

  const handleSortByDirectionChange = (event: React.ChangeEvent<{ value: 'ASC' | 'DESC' }>) => {
    dispatch(setExpansionSortDirection(event.target.value));
  };

  return (
    <StyledSortSelector>
      <SortByOption value={sortExpansionBy} onChange={handleSortByChange} variant="outlined">
        {expansionSortByOptions.map((sortByOption) => (
          <MenuItem key={sortByOption.label} value={sortByOption.value}>
            Sort by {sortByOption.label}
          </MenuItem>
        ))}
      </SortByOption>
      <SortByDirection value={sortExpansionByDirection} onChange={handleSortByDirectionChange} variant="outlined">
        <MenuItem value="ASC">ASC</MenuItem>
        <MenuItem value="DESC">DESC</MenuItem>
      </SortByDirection>
    </StyledSortSelector>
  );
};

const StyledSortSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
  width: '100%',
}));

const SortByOption = styled(Select)(() => ({
  width: '65%',
  marginRight: '8px',
}));

const SortByDirection = styled(Select)(() => ({
  width: 'calc(35% - 8px)',
}));

export default SetSortSelector;
