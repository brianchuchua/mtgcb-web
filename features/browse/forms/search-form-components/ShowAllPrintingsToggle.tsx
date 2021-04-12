import { useSelector, useDispatch } from 'react-redux';
import styled from 'styled-components';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import FormGroup from '@material-ui/core/FormGroup';
import Paper from '@material-ui/core/Paper';
import Switch from '@material-ui/core/Switch';
import { RootState } from '../../../../redux/rootReducer';
import { setShowAllPrintings } from '../../browseSlice';

const ShowAllPrintingsToggle: React.FC = () => {
  const dispatch = useDispatch();
  const { showAllPrintings } = useSelector((state: RootState) => state.browse);

  const updateShowAllPrintings = (toggleValue) => {
    dispatch(setShowAllPrintings(toggleValue));
  };

  const handleShowAllPrintingsChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    updateShowAllPrintings(event.target.checked);
  };

  return (
    <ToggleGroupWrapper variant="outlined">
      <FormGroup>
        <StyledFormControlLabel
          control={<Switch checked={showAllPrintings} onChange={handleShowAllPrintingsChange} name="showAllPrintings" color="primary" />}
          label="Show All Unique Printings"
        />
      </FormGroup>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '8px', marginTop: '0px' }));
const StyledFormControlLabel = styled(FormControlLabel)(() => ({ display: 'flex', justifyContent: 'center' }));

export default ShowAllPrintingsToggle;
