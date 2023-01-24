import FormControlLabel from '@material-ui/core/FormControlLabel';
import FormGroup from '@material-ui/core/FormGroup';
import Paper from '@material-ui/core/Paper';
import Switch from '@material-ui/core/Switch';
import Tooltip from '@material-ui/core/Tooltip';
import InfoOutlined from '@material-ui/icons/InfoOutlined';
import styled from 'styled-components';

interface IncludeSubsetsInSetsToggleProps {
  includeSubsetsInSets: boolean;
  updateIncludeSubsetsInSets: (showAllPrintings: boolean) => void;
}

const IncludeSubsetsInSetsToggle: React.FC<IncludeSubsetsInSetsToggleProps> = ({ includeSubsetsInSets, updateIncludeSubsetsInSets }) => {
  const handleIncludeSubsetsInSetsChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    updateIncludeSubsetsInSets(event.target.checked);
  };

  return (
    <ToggleGroupWrapper variant="outlined">
      <FormGroup>
        <StyledFormControlLabel
          control={
            <Switch
              checked={includeSubsetsInSets}
              onChange={handleIncludeSubsetsInSetsChange}
              name="includeSubsetsInSets"
              color="primary"
            />
          }
          label={
            <div style={{ display: 'flex', alignItems: 'center', flexWrap: 'wrap' }}>
              Track Subsets w/Main Set
              <Tooltip
                placement="right"
                title={
                  <>
                    This will show the percentage, value and cost to complete of a set <em>including any of its subsets</em>.<br />
                    <br />
                    For example, the set Tempest would show 0/351 cards instead of 0/350 because it would also include its Prerelease Promo
                    subset.
                    <br />
                    <br />
                    The buy links will also include purchases for any cards of this set's subsets.
                    <br /> <br />
                    This setting is off by default.
                  </>
                }
              >
                <InfoOutlined color="disabled" style={{ marginLeft: '5px' }} />
              </Tooltip>
            </div>
          }
        />
      </FormGroup>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '8px', marginTop: '0px' }));
const StyledFormControlLabel = styled(FormControlLabel)(() => ({ display: 'flex', justifyContent: 'left', margin: '0' }));

export default IncludeSubsetsInSetsToggle;
