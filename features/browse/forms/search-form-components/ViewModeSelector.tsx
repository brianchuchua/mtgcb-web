import Paper from '@material-ui/core/Paper';
import TableIcon from '@material-ui/icons/TableChart';
import ImagesIcon from '@material-ui/icons/ViewComfy';
import ToggleButton from '@material-ui/lab/ToggleButton';
import ToggleButtonGroup from '@material-ui/lab/ToggleButtonGroup';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { RootState } from '../../../../redux/rootReducer';
import { setViewMode } from '../../browseSlice';

const ViewModeSelector: React.FC = () => {
  const dispatch = useDispatch();
  const { viewMode } = useSelector((state: RootState) => state.browse);

  const updateViewMode = (viewModeValue) => {
    dispatch(setViewMode(viewModeValue));
  };

  const handleViewModeChange = (event: React.MouseEvent<HTMLElement>, newViewMode: string | null) => {
    updateViewMode(newViewMode);
  };

  return (
    <ToggleGroupWrapper>
      <StyledToggleButtonGroup value={viewMode} onChange={handleViewModeChange} exclusive aria-label="view as images or table">
        <StyledToggleButton value="images" aria-label="images">
          <ImagesIcon style={{ marginRight: '3px' }} /> Grid View
        </StyledToggleButton>
        <StyledToggleButton value="table" aria-label="table">
          <TableIcon style={{ marginRight: '3px' }} /> Table View
        </StyledToggleButton>
      </StyledToggleButtonGroup>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '10px', marginTop: '0px' }));
const StyledToggleButton = styled(ToggleButton)(() => ({ alignItems: 'end', justifyContent: 'center', width: '100%' }));
const StyledToggleButtonGroup = styled(ToggleButtonGroup)(() => ({ width: '100%' }));

export default ViewModeSelector;
